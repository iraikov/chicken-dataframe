# dataframe

Tabular data structure implementation for data analysis.

## Documentation

The `dataframe` library provides an interface for representing
numerical data in tables with rows and columns.  It is inspired by the
various dataframe implementations found in R, Python and Racket.

The `dataframe` library also provides functions for loading and saving
data from data frames as well as routines for descriptive statistics
and linear regression.

### Columns

Each dataframe consists of a collection of columns, which in turn is
an object consisting of a unique key, data collection, and an
associative list of properties. The following operations are defined
on columns.

<procedure>(column? obj)</procedure>
Returns true if the given object is a column.

<procedure>(get-column-properties column)</procedure>
Returns an associative list with column properties.

<procedure>(get-column-key column)</procedure>
Returns the key of the column.

<procedure>(get-column-collection column)</procedure>
Returns the data collection of the column.

<procedure>(column-deserialize column port)</procedure>
Loads the data collection of a column from the given port.

<procedure>(column-serialize column port)</procedure>
Stores the data collection of a column to the given port in an s-expression format.

### Creating data frames

<procedure>(make-data-frame [column-key-compare:
compare-symbol])</procedure> Creates a new dataframe, with optional
argument a procedure that specifies how to compare column
keys. Default is comparison on symbols. Returns the new dataframe.

<procedure>(df-insert-column df key collection properties)</procedure>
Inserts a new column with the given key, data collection, and
properties. Returns a new dataframe with the inserted column.

<procedure>(df-insert-derived df parent-key key proc
properties)</procedure> Inserts a derived column, that is a column
whose data elements are obtained by mapping a procedure onto the
elements of an existing (parent) column. Returns a new dataframe with
the inserted column.

<procedure>(df-insert-columns df lseq)</procedure>
Inserts the columns contained in the given lseq of column objects.

### Accessing data frames

<procedure>(show df)</procedure>
Displays a subset of the rows and columns contained in the dataframe.

<procedure>(row-count df)</procedure>
Returns the number of rows in the dataframe.

<procedure>(df-column df key)</procedure>
Returns the column indicated by the given key.

<procedure>(df-columns df)</procedure>
Returns a lazy sequence containing the columns of the dataframe.

<procedure>(df-filter-columns df proc)</procedure>
Returns a filtered lseq of the columns of the dataframe according to the given filter predicate procedure.

<procedure>(df-select-columns df keys)</procedure>
Returns an lseq of the columns of the dataframe that have the keys enumerated in the given list of keys.

<procedure>(df-keys df)</procedure>
Returns the keys of all columns in the dataframe.

<procedure>(df-items df)</procedure>
Returns an lseq of the key-column pairs contained in the dataframe.

<procedure>(apply-collections proc df key ...)</procedure>
Applies the given procedure to the data collections of the named columns of
the dataframe and returns the result as a list.

<procedure>(apply-columns proc df key ...)</procedure>
Applies the given procedure to the named columns of the dataframe and returns the result as a list.

<procedure>(map-collections proc df key ...)</procedure>
Applies the given procedure to the data collections of the named columns of the dataframe and returns the result as a dataframe.

<procedure>(map-columns proc df key ...)</procedure>
Applies the given procedure to the named columns of the dataframe and returns the result as a dataframe.

<procedure>(reduce-collections proc df seed key ...)</procedure>
Fold over the data collections of the named columns.

### Iterators

<procedure>(df-for-each-column df proc)</procedure>
Applies proc to each column.

<procedure>(df-for-each-collection df proc)</procedure>
Applies proc to the data collection of each column.

<procedure>(df-gen-rows df)</procedure>
Returns a generator procedure that returns the dataframe rows in succession.

<procedure>(df-gen-columns df)</procedure>
Returns a generator procedure the returns the dataframe columns in succession.

### Descriptive statistics

<procedure>(describe df port)</procedure>
Displays a table with the min/max/mean/sdev of each column in the dataframe.

<procedure>(cmin df)</procedure>
Computes the minimum value of each column.

<procedure>(cmax df)</procedure>
Computes the maximum value of each column.

<procedure>(mean df)</procedure>
Computes the mean value of each column.

<procedure>(median df)</procedure>
Computes the median value of each column.

<procedure>(mode df)</procedure>
Computes the mode value of each column.

<procedure>(range df)</procedure>
Computes the difference between maximum and minimum value of each column.

<procedure>(percentile df)</procedure>
Computes the percentile values of each column.

<procedure>(variance df)</procedure>
Computes the variance of each column.

<procedure>(standard-deviation df)</procedure>
Computes the standard deviation of each column.

<procedure>(coefficient-of-variation df)</procedure>
Computes the coefficient of variation of each column.

### Regression and correlation

<procedure>(linear-regression df x y)</procedure>
Linear regression between columns x and y.

<procedure>(correlation-coefficient df x y)</procedure>
Correlation coefficient between columns x and y.

### I/O

<procedure>(df-serialize df port)</procedure>
Stores the dataframe in an s-expression format to the given port.

<procedure>(df-deserialize df port)</procedure>
Loads the data collections of the dataframe columns from the given port.

## Examples

```scheme

(import scheme yasos dataframe dataframe-statistics)

(define df (make-data-frame))

(define df1
  (df-insert-column 
   df
   'base
   (list-tabulate 100 (lambda (x) (- x 10)))
   '()))


;;  exponential series
(define df2
  (df-insert-derived
   df1 'base 'exp
   (lambda (x) (* 2.0 (exp (* 0.1 x))))
   '()
   ))

(show df2 #f)
(describe df2 #f)

(linear-regression df2 'base 'exp)

```

## License

>
> Copyright 2019 Ivan Raikov
> 
>  This program is free software: you can redistribute it and/or modify
>  it under the terms of the GNU General Public License as published by
>  the Free Software Foundation, either version 3 of the License, or (at
>  your option) any later version.
>  
>  This program is distributed in the hope that it will be useful, but
>  WITHOUT ANY WARRANTY; without even the implied warranty of
>  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
>  General Public License for more details.
> 
>  A full copy of the GPL license can be found at
>  <http://www.gnu.org/licenses/>.

