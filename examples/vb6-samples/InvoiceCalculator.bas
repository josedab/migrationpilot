Attribute VB_Name = "InvoiceCalculator"
Option Explicit

' Invoice Calculator Module
' MigrationPilot VB6 Example
'
' This module calculates invoice totals, taxes, and discounts

' Constants
Private Const TAX_RATE As Double = 0.0825  ' 8.25% tax rate
Private Const EARLY_PAY_DISCOUNT As Double = 0.02  ' 2% early payment discount
Private Const BULK_DISCOUNT_THRESHOLD As Long = 1000
Private Const BULK_DISCOUNT_RATE As Double = 0.05  ' 5% bulk discount

' Custom type for invoice line items
Private Type LineItem
    ItemCode As String
    Description As String
    Quantity As Long
    UnitPrice As Currency
    DiscountPercent As Double
End Type

' Custom type for invoice
Private Type Invoice
    InvoiceNumber As String
    CustomerID As String
    InvoiceDate As Date
    DueDate As Date
    Items(1 To 100) As LineItem
    ItemCount As Integer
    SubTotal As Currency
    TaxAmount As Currency
    DiscountAmount As Currency
    TotalAmount As Currency
    IsPaid As Boolean
End Type

' Module-level variables
Private m_CurrentInvoice As Invoice
Private m_CustomerType As String  ' "RETAIL", "WHOLESALE", "PREFERRED"

'=============================================================================
' Function: CalculateLineTotal
' Business Rule: Line total = Quantity * Unit Price * (1 - Discount%)
'=============================================================================
Public Function CalculateLineTotal(item As LineItem) As Currency
    Dim baseAmount As Currency
    Dim discountAmount As Currency
    
    ' Calculate base amount
    baseAmount = item.Quantity * item.UnitPrice
    
    ' Apply line item discount
    If item.DiscountPercent > 0 Then
        discountAmount = baseAmount * item.DiscountPercent
        CalculateLineTotal = baseAmount - discountAmount
    Else
        CalculateLineTotal = baseAmount
    End If
End Function

'=============================================================================
' Function: CalculateSubTotal
' Business Rule: Sum of all line item totals
'=============================================================================
Public Function CalculateSubTotal(inv As Invoice) As Currency
    Dim i As Integer
    Dim total As Currency
    
    total = 0
    
    For i = 1 To inv.ItemCount
        total = total + CalculateLineTotal(inv.Items(i))
    Next i
    
    CalculateSubTotal = total
End Function

'=============================================================================
' Function: CalculateBulkDiscount
' Business Rule: 5% discount if subtotal exceeds $1000
' Wholesale customers get additional 3%
' Preferred customers get additional 5%
'=============================================================================
Public Function CalculateBulkDiscount(subTotal As Currency, customerType As String) As Currency
    Dim discountRate As Double
    
    ' Check if eligible for bulk discount
    If subTotal < BULK_DISCOUNT_THRESHOLD Then
        CalculateBulkDiscount = 0
        Exit Function
    End If
    
    ' Base bulk discount
    discountRate = BULK_DISCOUNT_RATE
    
    ' Additional discount by customer type
    Select Case UCase(customerType)
        Case "WHOLESALE"
            discountRate = discountRate + 0.03
        Case "PREFERRED"
            discountRate = discountRate + 0.05
        Case "RETAIL"
            ' No additional discount
        Case Else
            ' Unknown type - no additional discount
    End Select
    
    CalculateBulkDiscount = subTotal * discountRate
End Function

'=============================================================================
' Function: CalculateTax
' Business Rule: Tax = (SubTotal - Discounts) * Tax Rate
' Tax-exempt items (starting with "TX") are excluded
'=============================================================================
Public Function CalculateTax(inv As Invoice) As Currency
    Dim i As Integer
    Dim taxableAmount As Currency
    Dim lineTotal As Currency
    
    taxableAmount = 0
    
    For i = 1 To inv.ItemCount
        ' Check if item is tax-exempt
        If Left(inv.Items(i).ItemCode, 2) <> "TX" Then
            lineTotal = CalculateLineTotal(inv.Items(i))
            taxableAmount = taxableAmount + lineTotal
        End If
    Next i
    
    ' Subtract any discounts from taxable amount
    taxableAmount = taxableAmount - inv.DiscountAmount
    
    ' Ensure taxable amount is not negative
    If taxableAmount < 0 Then
        taxableAmount = 0
    End If
    
    CalculateTax = taxableAmount * TAX_RATE
End Function

'=============================================================================
' Function: CalculateEarlyPayDiscount
' Business Rule: 2% discount if paid within 10 days
' Only applies to invoices over $500
'=============================================================================
Public Function CalculateEarlyPayDiscount(inv As Invoice, paymentDate As Date) As Currency
    Dim daysDiff As Long
    
    ' Check minimum amount
    If inv.SubTotal < 500 Then
        CalculateEarlyPayDiscount = 0
        Exit Function
    End If
    
    ' Calculate days between invoice date and payment date
    daysDiff = DateDiff("d", inv.InvoiceDate, paymentDate)
    
    ' Apply discount if within 10 days
    If daysDiff <= 10 Then
        CalculateEarlyPayDiscount = inv.SubTotal * EARLY_PAY_DISCOUNT
    Else
        CalculateEarlyPayDiscount = 0
    End If
End Function

'=============================================================================
' Function: CalculateInvoiceTotal
' Business Rule: Total = SubTotal - Discounts + Tax
'=============================================================================
Public Function CalculateInvoiceTotal(inv As Invoice) As Currency
    Dim subTotal As Currency
    Dim bulkDiscount As Currency
    Dim taxAmount As Currency
    
    ' Calculate subtotal
    subTotal = CalculateSubTotal(inv)
    inv.SubTotal = subTotal
    
    ' Calculate bulk discount
    bulkDiscount = CalculateBulkDiscount(subTotal, m_CustomerType)
    inv.DiscountAmount = bulkDiscount
    
    ' Calculate tax
    taxAmount = CalculateTax(inv)
    inv.TaxAmount = taxAmount
    
    ' Calculate total
    CalculateInvoiceTotal = subTotal - bulkDiscount + taxAmount
End Function

'=============================================================================
' Sub: ProcessPayment
' Business Rule: Apply payment and calculate any early pay discount
'=============================================================================
Public Sub ProcessPayment(inv As Invoice, paymentAmount As Currency, paymentDate As Date)
    Dim earlyDiscount As Currency
    Dim amountDue As Currency
    
    ' Calculate amount due
    amountDue = inv.TotalAmount
    
    ' Check for early payment discount
    earlyDiscount = CalculateEarlyPayDiscount(inv, paymentDate)
    amountDue = amountDue - earlyDiscount
    
    ' Validate payment
    If paymentAmount < amountDue Then
        Err.Raise vbObjectError + 1, "InvoiceCalculator", _
            "Payment amount (" & Format(paymentAmount, "Currency") & _
            ") is less than amount due (" & Format(amountDue, "Currency") & ")"
    End If
    
    ' Mark as paid
    inv.IsPaid = True
End Sub

'=============================================================================
' Function: ValidateInvoice
' Business Rule: Invoice must have at least one item, valid dates, and customer
'=============================================================================
Public Function ValidateInvoice(inv As Invoice) As Boolean
    ' Check for items
    If inv.ItemCount < 1 Then
        ValidateInvoice = False
        Exit Function
    End If
    
    ' Check customer ID
    If Len(Trim(inv.CustomerID)) = 0 Then
        ValidateInvoice = False
        Exit Function
    End If
    
    ' Check dates
    If inv.DueDate < inv.InvoiceDate Then
        ValidateInvoice = False
        Exit Function
    End If
    
    ' Check invoice number
    If Len(Trim(inv.InvoiceNumber)) = 0 Then
        ValidateInvoice = False
        Exit Function
    End If
    
    ValidateInvoice = True
End Function
