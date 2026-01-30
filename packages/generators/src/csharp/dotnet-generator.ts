/**
 * C#/.NET Core Code Generator
 * 
 * Generates idiomatic C# code using ASP.NET Core web framework.
 */

import { BaseGenerator } from '../base-generator.js';
import type {
  GeneratedFile,
  ModuleDesign,
  GeneratorBusinessRule,
  GeneratorDataStructure,
} from '../types.js';

export class DotNetCoreGenerator extends BaseGenerator {
  
  protected getFileExtension(): string {
    return '.cs';
  }

  protected mapType(legacyType: string): string {
    const typeMap: Record<string, string> = {
      'PIC 9': 'int',
      'PIC 99': 'int',
      'PIC 9(n)': 'long',
      'PIC S9': 'int',
      'PIC X': 'string',
      'PIC X(n)': 'string',
      'PIC 9V99': 'decimal',
      'COMP': 'int',
      'COMP-3': 'decimal',
      'INTEGER': 'int',
      'REAL': 'float',
      'DOUBLE PRECISION': 'double',
      'CHARACTER': 'string',
      'String': 'string',
      'Integer': 'int',
      'Currency': 'decimal',
      'Date': 'DateTime',
      'Boolean': 'bool',
    };
    return typeMap[legacyType] || 'object';
  }

  protected generateModels(structures: GeneratorDataStructure[]): GeneratedFile[] {
    const files: GeneratedFile[] = [];
    const namespace = this.config.packageName;

    // Generate entity classes
    for (const structure of structures) {
      const className = this.toPascalCase(structure.name);
      const content = this.generateEntityClass(structure, className, namespace);
      files.push({
        path: `${this.config.outputDir}/Models/Entities/${className}.cs`,
        content,
        type: 'model',
      });
    }

    // Generate DTOs
    for (const structure of structures) {
      const className = this.toPascalCase(structure.name);
      const content = this.generateDTOClasses(structure, className, namespace);
      files.push({
        path: `${this.config.outputDir}/Models/DTOs/${className}Dto.cs`,
        content,
        type: 'model',
      });
    }

    // Generate DbContext
    const dbContextContent = this.generateDbContext(structures, namespace);
    files.push({
      path: `${this.config.outputDir}/Data/ApplicationDbContext.cs`,
      content: dbContextContent,
      type: 'source',
    });

    return files;
  }

  private generateEntityClass(structure: GeneratorDataStructure, className: string, namespace: string): string {
    const fields = structure.fields || [];
    
    return `using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace ${namespace}.Models.Entities;

/// <summary>
/// ${structure.description || structure.name}
/// </summary>
/// <remarks>
/// Migrated from: ${structure.sourceLocation?.file || 'legacy code'}
/// Lines: ${structure.sourceLocation?.startLine || 'N/A'}-${structure.sourceLocation?.endLine || 'N/A'}
/// </remarks>
[Table("${this.toSnakeCase(structure.name)}")]
public class ${className}
{
    [Key]
    public Guid Id { get; set; } = Guid.NewGuid();

${fields.map(f => this.generateEntityProperty(f)).join('\n\n')}

    public DateTime CreatedAt { get; set; } = DateTime.UtcNow;
    
    public DateTime UpdatedAt { get; set; } = DateTime.UtcNow;
}
`;
  }

  private generateEntityProperty(field: { name: string; type: string; description?: string; nullable?: boolean }): string {
    const csType = this.mapType(field.type);
    const propName = this.toPascalCase(field.name);
    const columnName = this.toSnakeCase(field.name);
    const nullable = field.nullable !== false;
    const nullableMarker = nullable && csType !== 'string' ? '?' : '';
    
    const attributes: string[] = [];
    attributes.push(`    [Column("${columnName}")]`);
    
    if (!nullable) {
      attributes.push('    [Required]');
    }
    
    if (csType === 'string') {
      attributes.push('    [MaxLength(255)]');
    }

    return `${attributes.join('\n')}
    /// <summary>${field.description || field.name}</summary>
    public ${csType}${nullableMarker} ${propName} { get; set; }`;
  }

  private generateDTOClasses(structure: GeneratorDataStructure, className: string, namespace: string): string {
    const fields = structure.fields || [];
    
    return `using System;
using System.ComponentModel.DataAnnotations;

namespace ${namespace}.Models.DTOs;

/// <summary>
/// Response DTO for ${className}
/// </summary>
public record ${className}Response
{
    public Guid Id { get; init; }
${fields.map(f => this.generateDTOProperty(f)).join('\n')}
    public DateTime CreatedAt { get; init; }
    public DateTime UpdatedAt { get; init; }
}

/// <summary>
/// Request DTO for creating ${className}
/// </summary>
public record Create${className}Request
{
${fields.filter(f => f.name.toLowerCase() !== 'id').map(f => this.generateDTOProperty(f, true)).join('\n')}
}

/// <summary>
/// Request DTO for updating ${className}
/// </summary>
public record Update${className}Request
{
${fields.filter(f => f.name.toLowerCase() !== 'id').map(f => this.generateDTOProperty(f, false, true)).join('\n')}
}
`;
  }

  private generateDTOProperty(
    field: { name: string; type: string; description?: string; nullable?: boolean },
    required = false,
    optional = false
  ): string {
    const csType = this.mapType(field.type);
    const propName = this.toPascalCase(field.name);
    const nullable = optional || (field.nullable !== false && !required);
    const nullableMarker = nullable && csType !== 'string' ? '?' : '';
    
    const attributes: string[] = [];
    if (required && !field.nullable) {
      attributes.push('    [Required]');
    }

    const attrStr = attributes.length > 0 ? attributes.join('\n') + '\n' : '';
    return `${attrStr}    public ${csType}${nullableMarker} ${propName} { get; init; }`;
  }

  private generateDbContext(structures: GeneratorDataStructure[], namespace: string): string {
    return `using Microsoft.EntityFrameworkCore;
using ${namespace}.Models.Entities;

namespace ${namespace}.Data;

/// <summary>
/// Application database context
/// </summary>
/// <remarks>Generated by MigrationPilot</remarks>
public class ApplicationDbContext : DbContext
{
    public ApplicationDbContext(DbContextOptions<ApplicationDbContext> options)
        : base(options)
    {
    }

${structures.map(s => {
  const className = this.toPascalCase(s.name);
  const tableName = this.toPascalCase(s.name) + 's';
  return `    public DbSet<${className}> ${tableName} { get; set; } = null!;`;
}).join('\n')}

    protected override void OnModelCreating(ModelBuilder modelBuilder)
    {
        base.OnModelCreating(modelBuilder);

${structures.map(s => {
  const className = this.toPascalCase(s.name);
  return `        modelBuilder.Entity<${className}>(entity =>
        {
            entity.HasKey(e => e.Id);
            entity.Property(e => e.CreatedAt).HasDefaultValueSql("CURRENT_TIMESTAMP");
            entity.Property(e => e.UpdatedAt).HasDefaultValueSql("CURRENT_TIMESTAMP");
        });`;
}).join('\n\n')}
    }
}
`;
  }

  protected generateInterfaces(design: ModuleDesign): GeneratedFile[] {
    const namespace = this.config.packageName;
    const content = this.generateServiceInterfaces(design, namespace);
    return [{
      path: `${this.config.outputDir}/Services/Interfaces/IBusinessRulesService.cs`,
      content,
      type: 'interface',
    }];
  }

  private generateServiceInterfaces(design: ModuleDesign, namespace: string): string {
    return `using System.Threading.Tasks;
using ${namespace}.Models.DTOs;

namespace ${namespace}.Services.Interfaces;

/// <summary>
/// Service interfaces for business operations
/// </summary>
/// <remarks>Generated by MigrationPilot</remarks>

${design.services.map(s => {
  const interfaceName = `I${this.toPascalCase(s.name)}Service`;
  return `/// <summary>
/// Interface for ${s.name} service
/// </summary>
public interface ${interfaceName}
{
${(s.endpoints || []).map(e => {
  const methodName = this.toPascalCase(e.path.replace(/\//g, '_')) + 'Async';
  const param = e.requestBody ? `${e.requestBody} request` : '';
  return `    /// <summary>
    /// ${e.description}
    /// </summary>
    Task<${e.responseType}> ${methodName}(${param});`;
}).join('\n\n')}
}`;
}).join('\n\n')}
`;
  }

  protected generateServices(
    design: ModuleDesign,
    rules: GeneratorBusinessRule[]
  ): GeneratedFile[] {
    const files: GeneratedFile[] = [];
    const namespace = this.config.packageName;

    // Generate Program.cs
    const programContent = this.generateProgram(design, namespace);
    files.push({
      path: `${this.config.outputDir}/Program.cs`,
      content: programContent,
      type: 'source',
    });

    // Generate Controllers
    const controllerContent = this.generateController(design, namespace);
    files.push({
      path: `${this.config.outputDir}/Controllers/${this.toPascalCase(design.name)}Controller.cs`,
      content: controllerContent,
      type: 'controller',
    });

    // Generate Business Rules Service
    const rulesContent = this.generateBusinessRulesService(rules, namespace);
    files.push({
      path: `${this.config.outputDir}/Services/BusinessRulesService.cs`,
      content: rulesContent,
      type: 'service',
    });

    // Generate Repository
    const repoContent = this.generateRepository(design, namespace);
    files.push({
      path: `${this.config.outputDir}/Repositories/Repository.cs`,
      content: repoContent,
      type: 'repository',
    });

    // Generate .csproj file
    const csprojContent = this.generateCsproj(design);
    files.push({
      path: `${this.config.outputDir}/${this.toPascalCase(design.name)}.csproj`,
      content: csprojContent,
      type: 'config',
    });

    // Generate appsettings.json
    const appSettingsContent = this.generateAppSettings(design);
    files.push({
      path: `${this.config.outputDir}/appsettings.json`,
      content: appSettingsContent,
      type: 'config',
    });

    return files;
  }

  private generateProgram(design: ModuleDesign, namespace: string): string {
    return `using Microsoft.EntityFrameworkCore;
using ${namespace}.Data;
using ${namespace}.Services;
using ${namespace}.Services.Interfaces;
using ${namespace}.Repositories;

var builder = WebApplication.CreateBuilder(args);

// Add services to the container
builder.Services.AddControllers();
builder.Services.AddEndpointsApiExplorer();
builder.Services.AddSwaggerGen(c =>
{
    c.SwaggerDoc("v1", new() { Title = "${design.name} API", Version = "v1" });
});

// Database
builder.Services.AddDbContext<ApplicationDbContext>(options =>
    options.UseNpgsql(builder.Configuration.GetConnectionString("DefaultConnection")));

// Dependency Injection
builder.Services.AddScoped<IRepository, Repository>();
builder.Services.AddScoped<IBusinessRulesService, BusinessRulesService>();

var app = builder.Build();

// Configure the HTTP request pipeline
if (app.Environment.IsDevelopment())
{
    app.UseSwagger();
    app.UseSwaggerUI();
}

app.UseHttpsRedirection();
app.UseAuthorization();
app.MapControllers();

// Health check endpoint
app.MapGet("/health", () => Results.Ok(new { Status = "Healthy" }));

app.Run();
`;
  }

  private generateController(design: ModuleDesign, namespace: string): string {
    const controllerName = this.toPascalCase(design.name) + 'Controller';
    const endpoints = design.services.flatMap(s => s.endpoints || []);
    
    return `using Microsoft.AspNetCore.Mvc;
using ${namespace}.Models.DTOs;
using ${namespace}.Services.Interfaces;

namespace ${namespace}.Controllers;

/// <summary>
/// ${design.description}
/// </summary>
/// <remarks>Generated by MigrationPilot</remarks>
[ApiController]
[Route("api/[controller]")]
public class ${controllerName} : ControllerBase
{
    private readonly IBusinessRulesService _businessRulesService;
    private readonly ILogger<${controllerName}> _logger;

    public ${controllerName}(
        IBusinessRulesService businessRulesService,
        ILogger<${controllerName}> logger)
    {
        _businessRulesService = businessRulesService;
        _logger = logger;
    }

${endpoints.map(e => this.generateControllerAction(e)).join('\n\n')}
}
`;
  }

  private generateControllerAction(endpoint: { path: string; method: string; description: string; requestBody?: string; responseType: string }): string {
    const methodName = this.toPascalCase(endpoint.path.replace(/\//g, '_'));
    const httpMethod = endpoint.method.charAt(0) + endpoint.method.slice(1).toLowerCase();
    const param = endpoint.requestBody ? `[FromBody] ${endpoint.requestBody} request` : '';
    const serviceCall = endpoint.requestBody ? 'request' : '';
    
    return `    /// <summary>
    /// ${endpoint.description}
    /// </summary>
    [Http${httpMethod}("${endpoint.path}")]
    [ProducesResponseType(typeof(${endpoint.responseType}), StatusCodes.Status200OK)]
    [ProducesResponseType(StatusCodes.Status400BadRequest)]
    [ProducesResponseType(StatusCodes.Status500InternalServerError)]
    public async Task<ActionResult<${endpoint.responseType}>> ${methodName}Async(${param})
    {
        try
        {
            var result = await _businessRulesService.${methodName}Async(${serviceCall});
            return Ok(result);
        }
        catch (ArgumentException ex)
        {
            _logger.LogWarning(ex, "Invalid request for ${methodName}");
            return BadRequest(new { Error = ex.Message });
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error processing ${methodName}");
            return StatusCode(500, new { Error = "An unexpected error occurred" });
        }
    }`;
  }

  private generateBusinessRulesService(rules: GeneratorBusinessRule[], namespace: string): string {
    return `using Microsoft.Extensions.Logging;
using ${namespace}.Models.DTOs;
using ${namespace}.Repositories;
using ${namespace}.Services.Interfaces;

namespace ${namespace}.Services;

/// <summary>
/// Business Rules Service Implementation
/// </summary>
/// <remarks>
/// Contains implementations of all extracted business rules.
/// Each method is documented with its source location and rule ID.
/// Generated by MigrationPilot.
/// </remarks>
public class BusinessRulesService : IBusinessRulesService
{
    private readonly IRepository _repository;
    private readonly ILogger<BusinessRulesService> _logger;

    public BusinessRulesService(
        IRepository repository,
        ILogger<BusinessRulesService> logger)
    {
        _repository = repository;
        _logger = logger;
    }

${rules.map(r => this.generateCSharpRuleMethod(r)).join('\n\n')}
}
`;
  }

  private generateCSharpRuleMethod(rule: GeneratorBusinessRule): string {
    const methodName = this.toPascalCase(rule.name.replace(/\s+/g, '_'));
    const inputs = rule.inputs || [];
    const output = rule.outputs?.[0];
    
    const params = inputs.map(i => `${this.mapType(i.type)} ${this.toCamelCase(i.name)}`).join(', ');
    const returnType = output ? this.mapType(output.type) : 'void';
    const asyncReturnType = returnType === 'void' ? 'Task' : `Task<${returnType}>`;

    return `    /// <summary>
    /// ${rule.name}
    /// </summary>
    /// <remarks>
    /// ${rule.description}
    /// 
    /// Rule ID: ${rule.id}
    /// Source: ${rule.sourceLocation?.file || 'N/A'}:${rule.sourceLocation?.startLine || 'N/A'}-${rule.sourceLocation?.endLine || 'N/A'}
    /// Confidence: ${(rule.confidence * 100).toFixed(1)}%
    /// </remarks>
${inputs.map(i => `    /// <param name="${this.toCamelCase(i.name)}">${i.description || i.name}</param>`).join('\n')}
${output ? `    /// <returns>${output.description || output.name}</returns>` : ''}
    public async ${asyncReturnType} ${methodName}Async(${params})
    {
        _logger.LogDebug("Executing rule {RuleId}: {RuleName}", "${rule.id}", "${rule.name}");

        // Business logic from source:
        // ${rule.logic?.calculation?.split('\n').join('\n        // ') || 'No calculation defined'}

        // TODO: Implement extracted logic
        await Task.CompletedTask;
        ${returnType !== 'void' ? `return ${this.getCSharpDefaultValue(returnType)};` : ''}
    }`;
  }

  private generateRepository(_design: ModuleDesign, namespace: string): string {
    return `using Microsoft.EntityFrameworkCore;
using ${namespace}.Data;

namespace ${namespace}.Repositories;

/// <summary>
/// Repository interface
/// </summary>
public interface IRepository
{
    Task<T?> GetByIdAsync<T>(Guid id) where T : class;
    Task<IEnumerable<T>> GetAllAsync<T>(int skip = 0, int take = 100) where T : class;
    Task<T> CreateAsync<T>(T entity) where T : class;
    Task<T> UpdateAsync<T>(T entity) where T : class;
    Task DeleteAsync<T>(Guid id) where T : class;
    Task SaveChangesAsync();
}

/// <summary>
/// Generic repository implementation
/// </summary>
/// <remarks>Generated by MigrationPilot</remarks>
public class Repository : IRepository
{
    private readonly ApplicationDbContext _context;

    public Repository(ApplicationDbContext context)
    {
        _context = context;
    }

    public async Task<T?> GetByIdAsync<T>(Guid id) where T : class
    {
        return await _context.Set<T>().FindAsync(id);
    }

    public async Task<IEnumerable<T>> GetAllAsync<T>(int skip = 0, int take = 100) where T : class
    {
        return await _context.Set<T>()
            .Skip(skip)
            .Take(take)
            .ToListAsync();
    }

    public async Task<T> CreateAsync<T>(T entity) where T : class
    {
        await _context.Set<T>().AddAsync(entity);
        await SaveChangesAsync();
        return entity;
    }

    public async Task<T> UpdateAsync<T>(T entity) where T : class
    {
        _context.Set<T>().Update(entity);
        await SaveChangesAsync();
        return entity;
    }

    public async Task DeleteAsync<T>(Guid id) where T : class
    {
        var entity = await GetByIdAsync<T>(id);
        if (entity != null)
        {
            _context.Set<T>().Remove(entity);
            await SaveChangesAsync();
        }
    }

    public async Task SaveChangesAsync()
    {
        await _context.SaveChangesAsync();
    }
}
`;
  }

  private generateCsproj(design: ModuleDesign): string {
    const projectName = this.toPascalCase(design.name);
    
    return `<Project Sdk="Microsoft.NET.Sdk.Web">

  <PropertyGroup>
    <TargetFramework>net8.0</TargetFramework>
    <Nullable>enable</Nullable>
    <ImplicitUsings>enable</ImplicitUsings>
    <RootNamespace>${this.config.packageName}</RootNamespace>
    <AssemblyName>${projectName}</AssemblyName>
  </PropertyGroup>

  <ItemGroup>
    <PackageReference Include="Microsoft.AspNetCore.OpenApi" Version="8.0.0" />
    <PackageReference Include="Microsoft.EntityFrameworkCore" Version="8.0.0" />
    <PackageReference Include="Microsoft.EntityFrameworkCore.Design" Version="8.0.0">
      <IncludeAssets>runtime; build; native; contentfiles; analyzers; buildtransitive</IncludeAssets>
      <PrivateAssets>all</PrivateAssets>
    </PackageReference>
    <PackageReference Include="Npgsql.EntityFrameworkCore.PostgreSQL" Version="8.0.0" />
    <PackageReference Include="Swashbuckle.AspNetCore" Version="6.5.0" />
  </ItemGroup>

</Project>
`;
  }

  private generateAppSettings(design: ModuleDesign): string {
    return `{
  "Logging": {
    "LogLevel": {
      "Default": "Information",
      "Microsoft.AspNetCore": "Warning",
      "Microsoft.EntityFrameworkCore": "Warning"
    }
  },
  "AllowedHosts": "*",
  "ConnectionStrings": {
    "DefaultConnection": "Host=localhost;Database=${this.toSnakeCase(design.name)};Username=postgres;Password=postgres"
  }
}
`;
  }

  protected generateTests(
    _design: ModuleDesign,
    rules: GeneratorBusinessRule[]
  ): GeneratedFile[] {
    const namespace = this.config.packageName;
    const content = this.generateXUnitTests(rules, namespace);
    return [{
      path: `${this.config.outputDir}.Tests/Services/BusinessRulesServiceTests.cs`,
      content,
      type: 'test',
    }];
  }

  private generateXUnitTests(rules: GeneratorBusinessRule[], namespace: string): string {
    return `using Xunit;
using Moq;
using Microsoft.Extensions.Logging;
using ${namespace}.Services;
using ${namespace}.Repositories;

namespace ${namespace}.Tests.Services;

/// <summary>
/// Business Rules Service Tests
/// </summary>
/// <remarks>
/// Unit tests for validating behavioral equivalence.
/// Generated by MigrationPilot.
/// </remarks>
public class BusinessRulesServiceTests
{
    private readonly Mock<IRepository> _repositoryMock;
    private readonly Mock<ILogger<BusinessRulesService>> _loggerMock;
    private readonly BusinessRulesService _service;

    public BusinessRulesServiceTests()
    {
        _repositoryMock = new Mock<IRepository>();
        _loggerMock = new Mock<ILogger<BusinessRulesService>>();
        _service = new BusinessRulesService(_repositoryMock.Object, _loggerMock.Object);
    }

${rules.map(r => this.generateXUnitTestCase(r)).join('\n\n')}
}
`;
  }

  private generateXUnitTestCase(rule: GeneratorBusinessRule): string {
    const methodName = this.toPascalCase(rule.name.replace(/\s+/g, '_'));
    void methodName; // Used for test class naming

    return `    #region ${rule.name} Tests

    [Fact]
    public async Task ${methodName}_ShouldImplement${rule.id.replace(/-/g, '')}Correctly()
    {
        // Arrange
        // TODO: Set up test inputs

        // Act
        // var result = await _service.${methodName}Async(...);

        // Assert
        // Assert.Equal(expected, result);
    }

    [Fact]
    public async Task ${methodName}_ShouldHandleBoundaryValues()
    {
        // Arrange
        // Edge cases: ${rule.edgeCases?.join(', ') || 'None documented'}

        // Act

        // Assert
    }

    [Theory]
    [InlineData(/* TODO: Add test data */)]
    public async Task ${methodName}_ShouldProduceCorrectOutput(/* params */)
    {
        // Arrange

        // Act
        // var result = await _service.${methodName}Async(...);

        // Assert
        // Assert.Equal(expected, result);
    }

    #endregion`;
  }

  // Helper methods
  private toPascalCase(name: string): string {
    return name
      .replace(/[^a-zA-Z0-9]+/g, ' ')
      .split(' ')
      .map(word => word.charAt(0).toUpperCase() + word.slice(1).toLowerCase())
      .join('');
  }

  private toCamelCase(name: string): string {
    const pascal = this.toPascalCase(name);
    return pascal.charAt(0).toLowerCase() + pascal.slice(1);
  }

  private toSnakeCase(name: string): string {
    return name
      .replace(/[^a-zA-Z0-9]+/g, '_')
      .replace(/([a-z])([A-Z])/g, '$1_$2')
      .toLowerCase();
  }

  private getCSharpDefaultValue(type: string): string {
    const defaults: Record<string, string> = {
      'int': '0',
      'long': '0L',
      'float': '0f',
      'double': '0d',
      'decimal': '0m',
      'string': '""',
      'bool': 'false',
      'DateTime': 'DateTime.MinValue',
      'object': 'null!',
    };
    return defaults[type] || 'default!';
  }
}
