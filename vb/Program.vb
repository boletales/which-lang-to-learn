Imports System

Module Program
    Sub Main(args As String())
        Console.WriteLine("start")
        Const max  As Integer = 100000000
        Dim sieve() As Boolean = Enumerable.Repeat(True, max + 1).ToArray()
        Dim sqrtmax As Integer = CType(Math.Sqrt(max), Integer)
        For i As Integer = 2 To sqrtmax
            If sieve(i) Then
                For j As Integer= i * i To max Step i
                    sieve(j) = False
                Next
            End If
        Next
        Dim primes(max + 1) As Integer
        Dim pcount As Integer = 0
        For i As Integer = 2 To max
            If sieve(i) Then
                primes(pcount) = i
                pcount += 1
            End If
        Next
 
        Console.WriteLine(primes(pcount - 1))
 
        Console.WriteLine("end")
    End Sub
End Module
