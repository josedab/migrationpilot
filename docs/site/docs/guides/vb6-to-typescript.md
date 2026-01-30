# VB6 to TypeScript Migration Guide

This guide walks you through migrating a Visual Basic 6 application to modern TypeScript with NestJS.

## Prerequisites

- MigrationPilot installed and configured
- Node.js 18+ and npm/pnpm
- Access to VB6 source files (.bas, .cls, .frm)
- Basic understanding of the legacy system

## Sample VB6 Application

We'll migrate an inventory management module:

```vb
' Inventory.cls - Inventory management class
Option Explicit

Private mProductId As String
Private mQuantity As Long
Private mUnitPrice As Currency
Private mReorderLevel As Long

Public Property Get ProductId() As String
    ProductId = mProductId
End Property

Public Property Let ProductId(value As String)
    mProductId = value
End Property

Public Property Get Quantity() As Long
    Quantity = mQuantity
End Property

Public Property Let Quantity(value As Long)
    If value < 0 Then
        Err.Raise 1001, "Inventory", "Quantity cannot be negative"
    End If
    mQuantity = value
End Property

Public Property Get TotalValue() As Currency
    TotalValue = mQuantity * mUnitPrice
End Property

Public Function NeedsReorder() As Boolean
    NeedsReorder = mQuantity <= mReorderLevel
End Function

Public Sub AdjustStock(adjustment As Long)
    Dim newQty As Long
    newQty = mQuantity + adjustment
    
    If newQty < 0 Then
        Err.Raise 1002, "Inventory", "Insufficient stock"
    End If
    
    mQuantity = newQty
End Sub
```

## Step 1: Create a Project

```bash
migrationpilot project create inventory-system \
  --language vb6 \
  --target typescript \
  --description "Inventory management system modernization"
```

## Step 2: Upload Source Code

```bash
migrationpilot analyze ./vb6-src \
  --language vb6 \
  --output analysis.json \
  --verbose
```

### Analysis Output

```
📊 Analysis Summary
──────────────────────────────────────────────────
  Files Analyzed:     15
  Classes (*.cls):    8
  Modules (*.bas):    4
  Forms (*.frm):      3
  
📋 Business Rules Extracted
──────────────────────────────────────────────────
  BR-001: Stock Quantity Validation (95%)
  BR-002: Reorder Level Check (98%)
  BR-003: Total Value Calculation (100%)
  BR-004: Stock Adjustment (92%)
```

## Step 3: Run Migration

```bash
migrationpilot migrate \
  --project inventory-system \
  --target typescript \
  --output ./generated-ts
```

### Generated Project Structure

```
generated-ts/
├── package.json
├── tsconfig.json
├── nest-cli.json
├── src/
│   ├── main.ts
│   ├── app.module.ts
│   ├── inventory/
│   │   ├── inventory.module.ts
│   │   ├── inventory.controller.ts
│   │   ├── inventory.service.ts
│   │   ├── dto/
│   │   │   ├── create-inventory.dto.ts
│   │   │   ├── update-inventory.dto.ts
│   │   │   └── adjust-stock.dto.ts
│   │   ├── entities/
│   │   │   └── inventory.entity.ts
│   │   └── exceptions/
│   │       └── inventory.exceptions.ts
│   └── common/
│       └── filters/
│           └── http-exception.filter.ts
├── test/
│   └── inventory.e2e-spec.ts
└── docs/
    └── business-rules.md
```

## Step 4: Review Generated Code

### Entity (from VB6 Class)

```typescript
// src/inventory/entities/inventory.entity.ts

/**
 * Inventory entity representing product stock.
 * 
 * Migrated from: Inventory.cls
 * Business Rules: BR-001, BR-002, BR-003
 */
export class Inventory {
  private _productId: string;
  private _quantity: number;
  private _unitPrice: number;
  private _reorderLevel: number;

  constructor(
    productId: string,
    quantity: number = 0,
    unitPrice: number = 0,
    reorderLevel: number = 0
  ) {
    this._productId = productId;
    this.quantity = quantity; // Use setter for validation
    this._unitPrice = unitPrice;
    this._reorderLevel = reorderLevel;
  }

  get productId(): string {
    return this._productId;
  }

  set productId(value: string) {
    this._productId = value;
  }

  get quantity(): number {
    return this._quantity;
  }

  /**
   * Set quantity with validation.
   * 
   * Business Rule: BR-001 - Quantity cannot be negative
   * Source: Inventory.cls (lines 18-22)
   */
  set quantity(value: number) {
    if (value < 0) {
      throw new InvalidQuantityException('Quantity cannot be negative');
    }
    this._quantity = value;
  }

  get unitPrice(): number {
    return this._unitPrice;
  }

  set unitPrice(value: number) {
    this._unitPrice = value;
  }

  get reorderLevel(): number {
    return this._reorderLevel;
  }

  set reorderLevel(value: number) {
    this._reorderLevel = value;
  }

  /**
   * Calculate total inventory value.
   * 
   * Business Rule: BR-003 - Total Value Calculation
   * Source: Inventory.cls (lines 28-30)
   */
  get totalValue(): number {
    return this._quantity * this._unitPrice;
  }

  /**
   * Check if product needs reordering.
   * 
   * Business Rule: BR-002 - Reorder Level Check
   * Source: Inventory.cls (lines 32-34)
   */
  needsReorder(): boolean {
    return this._quantity <= this._reorderLevel;
  }

  /**
   * Adjust stock level.
   * 
   * Business Rule: BR-004 - Stock Adjustment
   * Source: Inventory.cls (lines 36-46)
   */
  adjustStock(adjustment: number): void {
    const newQuantity = this._quantity + adjustment;
    
    if (newQuantity < 0) {
      throw new InsufficientStockException(
        `Insufficient stock. Current: ${this._quantity}, Requested: ${Math.abs(adjustment)}`
      );
    }
    
    this._quantity = newQuantity;
  }
}
```

### Custom Exceptions

```typescript
// src/inventory/exceptions/inventory.exceptions.ts

import { HttpException, HttpStatus } from '@nestjs/common';

/**
 * Exception for invalid quantity values.
 * 
 * Maps to VB6 Err.Raise 1001
 */
export class InvalidQuantityException extends HttpException {
  constructor(message: string = 'Invalid quantity') {
    super(
      {
        statusCode: HttpStatus.BAD_REQUEST,
        error: 'InvalidQuantity',
        message,
        code: 1001,
      },
      HttpStatus.BAD_REQUEST
    );
  }
}

/**
 * Exception for insufficient stock.
 * 
 * Maps to VB6 Err.Raise 1002
 */
export class InsufficientStockException extends HttpException {
  constructor(message: string = 'Insufficient stock') {
    super(
      {
        statusCode: HttpStatus.CONFLICT,
        error: 'InsufficientStock',
        message,
        code: 1002,
      },
      HttpStatus.CONFLICT
    );
  }
}
```

### Service Layer

```typescript
// src/inventory/inventory.service.ts

import { Injectable, NotFoundException } from '@nestjs/common';
import { Inventory } from './entities/inventory.entity';
import { CreateInventoryDto } from './dto/create-inventory.dto';
import { AdjustStockDto } from './dto/adjust-stock.dto';

@Injectable()
export class InventoryService {
  private readonly inventory = new Map<string, Inventory>();

  create(dto: CreateInventoryDto): Inventory {
    const item = new Inventory(
      dto.productId,
      dto.quantity,
      dto.unitPrice,
      dto.reorderLevel
    );
    
    this.inventory.set(item.productId, item);
    return item;
  }

  findOne(productId: string): Inventory {
    const item = this.inventory.get(productId);
    if (!item) {
      throw new NotFoundException(`Product ${productId} not found`);
    }
    return item;
  }

  findAll(): Inventory[] {
    return Array.from(this.inventory.values());
  }

  findLowStock(): Inventory[] {
    return this.findAll().filter(item => item.needsReorder());
  }

  adjustStock(productId: string, dto: AdjustStockDto): Inventory {
    const item = this.findOne(productId);
    item.adjustStock(dto.adjustment);
    return item;
  }

  getTotalInventoryValue(): number {
    return this.findAll().reduce((sum, item) => sum + item.totalValue, 0);
  }
}
```

### Controller

```typescript
// src/inventory/inventory.controller.ts

import { Controller, Get, Post, Patch, Param, Body } from '@nestjs/common';
import { ApiTags, ApiOperation, ApiResponse } from '@nestjs/swagger';
import { InventoryService } from './inventory.service';
import { CreateInventoryDto } from './dto/create-inventory.dto';
import { AdjustStockDto } from './dto/adjust-stock.dto';

@ApiTags('inventory')
@Controller('inventory')
export class InventoryController {
  constructor(private readonly inventoryService: InventoryService) {}

  @Post()
  @ApiOperation({ summary: 'Create inventory item' })
  @ApiResponse({ status: 201, description: 'Item created' })
  create(@Body() dto: CreateInventoryDto) {
    return this.inventoryService.create(dto);
  }

  @Get()
  @ApiOperation({ summary: 'List all inventory items' })
  findAll() {
    return this.inventoryService.findAll();
  }

  @Get('low-stock')
  @ApiOperation({ summary: 'List items needing reorder' })
  findLowStock() {
    return this.inventoryService.findLowStock();
  }

  @Get('total-value')
  @ApiOperation({ summary: 'Get total inventory value' })
  getTotalValue() {
    return { totalValue: this.inventoryService.getTotalInventoryValue() };
  }

  @Get(':productId')
  @ApiOperation({ summary: 'Get inventory item by product ID' })
  findOne(@Param('productId') productId: string) {
    return this.inventoryService.findOne(productId);
  }

  @Patch(':productId/adjust')
  @ApiOperation({ summary: 'Adjust stock level' })
  @ApiResponse({ status: 200, description: 'Stock adjusted' })
  @ApiResponse({ status: 409, description: 'Insufficient stock' })
  adjustStock(
    @Param('productId') productId: string,
    @Body() dto: AdjustStockDto
  ) {
    return this.inventoryService.adjustStock(productId, dto);
  }
}
```

### DTOs

```typescript
// src/inventory/dto/create-inventory.dto.ts

import { ApiProperty } from '@nestjs/swagger';
import { IsString, IsNumber, Min, IsOptional } from 'class-validator';

export class CreateInventoryDto {
  @ApiProperty({ example: 'PROD-001' })
  @IsString()
  productId: string;

  @ApiProperty({ example: 100 })
  @IsNumber()
  @Min(0)
  quantity: number;

  @ApiProperty({ example: 29.99 })
  @IsNumber()
  @Min(0)
  unitPrice: number;

  @ApiProperty({ example: 10, required: false })
  @IsNumber()
  @Min(0)
  @IsOptional()
  reorderLevel?: number = 0;
}
```

## Step 5: Run Tests

```typescript
// test/inventory.e2e-spec.ts

import { Test } from '@nestjs/testing';
import { INestApplication } from '@nestjs/common';
import * as request from 'supertest';
import { AppModule } from '../src/app.module';

describe('InventoryController (e2e)', () => {
  let app: INestApplication;

  beforeEach(async () => {
    const moduleFixture = await Test.createTestingModule({
      imports: [AppModule],
    }).compile();

    app = moduleFixture.createNestApplication();
    await app.init();
  });

  afterEach(async () => {
    await app.close();
  });

  it('/inventory (POST) - create item', () => {
    return request(app.getHttpServer())
      .post('/inventory')
      .send({
        productId: 'TEST-001',
        quantity: 100,
        unitPrice: 10.00,
        reorderLevel: 20,
      })
      .expect(201)
      .expect((res) => {
        expect(res.body.productId).toBe('TEST-001');
        expect(res.body.totalValue).toBe(1000);
      });
  });

  it('/inventory/:id/adjust (PATCH) - adjust stock', async () => {
    // Create item first
    await request(app.getHttpServer())
      .post('/inventory')
      .send({ productId: 'TEST-002', quantity: 50, unitPrice: 5.00 });

    // Adjust stock
    return request(app.getHttpServer())
      .patch('/inventory/TEST-002/adjust')
      .send({ adjustment: -30 })
      .expect(200)
      .expect((res) => {
        expect(res.body.quantity).toBe(20);
      });
  });

  it('/inventory/:id/adjust (PATCH) - insufficient stock', async () => {
    await request(app.getHttpServer())
      .post('/inventory')
      .send({ productId: 'TEST-003', quantity: 10, unitPrice: 5.00 });

    return request(app.getHttpServer())
      .patch('/inventory/TEST-003/adjust')
      .send({ adjustment: -20 })
      .expect(409);
  });
});
```

Run tests:

```bash
cd generated-ts
npm install
npm run test:e2e
```

## Data Type Mapping

| VB6 | TypeScript |
|-----|------------|
| `String` | `string` |
| `Integer` | `number` |
| `Long` | `number` |
| `Single` | `number` |
| `Double` | `number` |
| `Currency` | `number` (use Decimal.js for precision) |
| `Boolean` | `boolean` |
| `Date` | `Date` |
| `Variant` | `any` or `unknown` |
| `Object` | `object` or specific type |
| `Collection` | `Array<T>` or `Map<K, V>` |

## Common Patterns

### VB6 Property → TypeScript Getter/Setter

```vb
Private mValue As Long

Public Property Get Value() As Long
    Value = mValue
End Property

Public Property Let Value(v As Long)
    mValue = v
End Property
```

```typescript
private _value: number;

get value(): number {
  return this._value;
}

set value(v: number) {
  this._value = v;
}
```

### VB6 Error Handling → TypeScript Exceptions

```vb
On Error GoTo ErrorHandler
' ... code ...
Exit Sub

ErrorHandler:
    Err.Raise 1001, "Module", "Error message"
```

```typescript
try {
  // ... code ...
} catch (error) {
  throw new CustomException('Error message');
}
```

### VB6 Collection → TypeScript Array/Map

```vb
Dim col As New Collection
col.Add item, key
For Each item In col
    ' ...
Next
```

```typescript
const map = new Map<string, Item>();
map.set(key, item);

for (const item of map.values()) {
  // ...
}
```

## Next Steps

- [Add database persistence](../deployment/cloud.md)
- [Set up authentication](../deployment/security.md)
- [Deploy to production](../deployment/kubernetes.md)
