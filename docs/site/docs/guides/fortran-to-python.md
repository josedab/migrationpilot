# Fortran to Python Migration Guide

This guide walks you through migrating Fortran scientific computing code to modern Python with NumPy and FastAPI.

## Prerequisites

- MigrationPilot installed and configured
- Python 3.10+ with pip
- Access to Fortran source files
- Basic understanding of the legacy system

## Sample Fortran Application

We'll migrate a numerical integration program:

```fortran
      PROGRAM INTEGRATE
C     Numerical integration using Simpson's rule
      IMPLICIT NONE
      REAL*8 A, B, RESULT
      INTEGER N
      REAL*8 FUNC
      EXTERNAL FUNC
      
      A = 0.0D0
      B = 3.14159265D0
      N = 100
      
      CALL SIMPSON(FUNC, A, B, N, RESULT)
      WRITE(*,*) 'Integral result: ', RESULT
      
      END

      SUBROUTINE SIMPSON(F, A, B, N, RESULT)
      IMPLICIT NONE
      REAL*8 F, A, B, RESULT
      INTEGER N
      REAL*8 H, X, SUM
      INTEGER I
      EXTERNAL F
      
      H = (B - A) / N
      SUM = F(A) + F(B)
      
      DO I = 1, N-1
          X = A + I * H
          IF (MOD(I, 2) .EQ. 0) THEN
              SUM = SUM + 2.0D0 * F(X)
          ELSE
              SUM = SUM + 4.0D0 * F(X)
          END IF
      END DO
      
      RESULT = SUM * H / 3.0D0
      
      END
```

## Step 1: Create a Project

```bash
migrationpilot project create scientific-compute \
  --language fortran \
  --target python \
  --description "Scientific computing library modernization"
```

## Step 2: Upload Source Code

```bash
# Analyze and upload Fortran files
migrationpilot analyze ./fortran-src \
  --language fortran \
  --output analysis.json \
  --verbose
```

### Analysis Output

```
📊 Analysis Summary
──────────────────────────────────────────────────
  Files Analyzed:     8
  Subroutines:        24
  Functions:          12
  COMMON Blocks:      5
  
📋 Complexity Metrics
──────────────────────────────────────────────────
  SIMPSON:        Complexity 8  (Low)
  GAUSS_QUAD:     Complexity 15 (Medium)
  MATRIX_MULT:    Complexity 22 (High)
```

## Step 3: Run Migration

```bash
migrationpilot migrate \
  --project scientific-compute \
  --target python \
  --output ./generated-python
```

### Generated Project Structure

```
generated-python/
├── pyproject.toml
├── src/
│   └── scientific_compute/
│       ├── __init__.py
│       ├── integration/
│       │   ├── __init__.py
│       │   ├── simpson.py
│       │   └── quadrature.py
│       ├── linear_algebra/
│       │   ├── __init__.py
│       │   └── matrix.py
│       └── api/
│           ├── __init__.py
│           └── main.py
├── tests/
│   ├── test_integration.py
│   └── test_matrix.py
└── docs/
    └── api.md
```

## Step 4: Review Generated Code

### Simpson Integration (Python)

```python
"""
Simpson's Rule Numerical Integration

Migrated from: SIMPSON.f (lines 18-42)
Business Rule: BR-001 - Numerical Integration
"""

import numpy as np
from typing import Callable


def simpson(
    f: Callable[[float], float],
    a: float,
    b: float,
    n: int = 100
) -> float:
    """
    Compute definite integral using Simpson's rule.
    
    Args:
        f: Function to integrate
        a: Lower bound
        b: Upper bound
        n: Number of intervals (must be even)
    
    Returns:
        Approximate value of the integral
        
    Raises:
        ValueError: If n is not positive and even
    """
    if n <= 0 or n % 2 != 0:
        raise ValueError("n must be a positive even integer")
    
    h = (b - a) / n
    x = np.linspace(a, b, n + 1)
    y = np.array([f(xi) for xi in x])
    
    # Simpson's rule: (h/3) * [f(a) + 4*sum(odd) + 2*sum(even) + f(b)]
    result = y[0] + y[-1]
    result += 4 * np.sum(y[1:-1:2])  # Odd indices
    result += 2 * np.sum(y[2:-1:2])  # Even indices
    
    return result * h / 3


def simpson_vectorized(
    f: Callable[[np.ndarray], np.ndarray],
    a: float,
    b: float,
    n: int = 100
) -> float:
    """
    Vectorized Simpson's rule for better performance.
    
    Uses NumPy broadcasting for efficient computation.
    """
    if n <= 0 or n % 2 != 0:
        raise ValueError("n must be a positive even integer")
    
    h = (b - a) / n
    x = np.linspace(a, b, n + 1)
    y = f(x)
    
    coefficients = np.ones(n + 1)
    coefficients[1:-1:2] = 4  # Odd
    coefficients[2:-1:2] = 2  # Even
    
    return np.dot(coefficients, y) * h / 3
```

### FastAPI Endpoint

```python
"""
API endpoints for scientific computing.
"""

from fastapi import FastAPI, HTTPException
from pydantic import BaseModel, Field
import numpy as np

from ..integration.simpson import simpson_vectorized


app = FastAPI(
    title="Scientific Compute API",
    description="Numerical computing services migrated from Fortran",
    version="1.0.0"
)


class IntegrationRequest(BaseModel):
    """Request model for numerical integration."""
    
    function: str = Field(
        description="Function to integrate (Python expression)",
        example="np.sin(x)"
    )
    lower_bound: float = Field(description="Lower integration bound")
    upper_bound: float = Field(description="Upper integration bound")
    intervals: int = Field(
        default=100,
        ge=2,
        description="Number of intervals (must be even)"
    )


class IntegrationResponse(BaseModel):
    """Response model for integration result."""
    
    result: float
    method: str = "simpson"
    intervals: int
    bounds: tuple[float, float]


@app.post("/api/integrate", response_model=IntegrationResponse)
async def integrate(request: IntegrationRequest):
    """
    Compute definite integral using Simpson's rule.
    
    Migrated from: INTEGRATE program (SIMPSON.f)
    """
    try:
        # Create function from expression (sanitized)
        allowed_names = {"np": np, "sin": np.sin, "cos": np.cos, 
                        "exp": np.exp, "log": np.log, "sqrt": np.sqrt}
        
        def f(x):
            return eval(request.function, {"__builtins__": {}}, 
                       {**allowed_names, "x": x})
        
        result = simpson_vectorized(
            f,
            request.lower_bound,
            request.upper_bound,
            request.intervals
        )
        
        return IntegrationResponse(
            result=float(result),
            intervals=request.intervals,
            bounds=(request.lower_bound, request.upper_bound)
        )
        
    except Exception as e:
        raise HTTPException(status_code=400, detail=str(e))
```

## Step 5: Run Tests

### Generated Test Suite

```python
"""
Tests for Simpson integration.

Migrated from: TEST_SIMPSON.f
"""

import pytest
import numpy as np
from scientific_compute.integration.simpson import simpson, simpson_vectorized


class TestSimpsonIntegration:
    """Test cases for Simpson's rule implementation."""
    
    def test_constant_function(self):
        """Integral of constant should be (b-a) * c."""
        result = simpson(lambda x: 5.0, 0, 10, 100)
        assert np.isclose(result, 50.0, rtol=1e-10)
    
    def test_linear_function(self):
        """Integral of x from 0 to 1 should be 0.5."""
        result = simpson(lambda x: x, 0, 1, 100)
        assert np.isclose(result, 0.5, rtol=1e-10)
    
    def test_quadratic_function(self):
        """Integral of x^2 from 0 to 1 should be 1/3."""
        result = simpson(lambda x: x**2, 0, 1, 100)
        assert np.isclose(result, 1/3, rtol=1e-6)
    
    def test_sine_function(self):
        """Integral of sin(x) from 0 to pi should be 2."""
        result = simpson(np.sin, 0, np.pi, 100)
        assert np.isclose(result, 2.0, rtol=1e-6)
    
    def test_invalid_intervals(self):
        """Should raise error for invalid interval count."""
        with pytest.raises(ValueError):
            simpson(lambda x: x, 0, 1, -1)
        with pytest.raises(ValueError):
            simpson(lambda x: x, 0, 1, 3)  # Odd number
    
    def test_vectorized_matches_scalar(self):
        """Vectorized version should match scalar version."""
        f = np.sin
        result_scalar = simpson(f, 0, np.pi, 100)
        result_vectorized = simpson_vectorized(f, 0, np.pi, 100)
        assert np.isclose(result_scalar, result_vectorized, rtol=1e-10)
    
    @pytest.mark.parametrize("n", [10, 50, 100, 500, 1000])
    def test_convergence(self, n):
        """Error should decrease as n increases."""
        exact = 2.0  # Integral of sin(x) from 0 to pi
        result = simpson(np.sin, 0, np.pi, n)
        error = abs(result - exact)
        # Simpson's rule has O(h^4) error
        assert error < 10 / n**4
```

Run the tests:

```bash
cd generated-python
pytest tests/ -v
```

## Step 6: Validate Equivalence

```bash
migrationpilot validate --project scientific-compute --verbose
```

## Data Type Mapping

| Fortran | Python |
|---------|--------|
| `INTEGER` | `int` |
| `REAL` | `float` |
| `REAL*8` / `DOUBLE PRECISION` | `float` or `np.float64` |
| `COMPLEX` | `complex` |
| `CHARACTER*n` | `str` |
| `LOGICAL` | `bool` |
| Arrays | `np.ndarray` |
| `COMMON` blocks | Module-level variables or classes |

## Common Patterns

### DO Loops → Python Loops/NumPy

```fortran
DO I = 1, N
    A(I) = A(I) * 2.0
END DO
```

```python
# Using NumPy (preferred)
a = a * 2.0

# Using loop (when needed)
for i in range(n):
    a[i] = a[i] * 2.0
```

### Fortran Arrays → NumPy Arrays

```fortran
REAL*8 A(100, 100)
A(I, J) = A(I, J) + B(I, J)
```

```python
import numpy as np
a = np.zeros((100, 100))
a = a + b  # or a += b
```

### SUBROUTINE → Function

```fortran
SUBROUTINE NORM(X, N, RESULT)
    REAL*8 X(N), RESULT
    RESULT = SQRT(SUM(X**2))
END
```

```python
def norm(x: np.ndarray) -> float:
    return np.sqrt(np.sum(x**2))
```

## Performance Considerations

### Use NumPy Vectorization

```python
# Slow: Python loop
result = 0
for i in range(len(x)):
    result += x[i] * y[i]

# Fast: NumPy
result = np.dot(x, y)
```

### Use Type Hints for Numba JIT

```python
import numba

@numba.jit(nopython=True)
def fast_simpson(y: np.ndarray, h: float) -> float:
    n = len(y) - 1
    result = y[0] + y[-1]
    for i in range(1, n):
        if i % 2 == 0:
            result += 2 * y[i]
        else:
            result += 4 * y[i]
    return result * h / 3
```

## Next Steps

- [Add API documentation with Swagger](/docs/api/overview) - API reference
- [Deploy to cloud](/docs/deployment/cloud) - Cloud deployment guide
- [View benchmarks](/docs/benchmarks) - Performance benchmarks
