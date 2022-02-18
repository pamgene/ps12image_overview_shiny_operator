# ps12image overview operator

#### Description

`ps12image overview` operator provides a view for filtering and viewing ps12 images.

##### Usage

Input projection|.
---|---
`col`           | is the documentId of the zip file containing the tiff images

Output relations|.
---|---
`Operator view` | view of the Shiny application

##### Details

This operator is expecting a documentId that references to a zip file. The zip file needs to contain a "ImageResults" directory where the Tiff images are stored. The operator will download the images and convert them to PNG, before showing them in a grid. Above the grid, there are a couple filters to determine which images will show up in the grid. These filters are:

* Cycle
* Exposure Time

##### See Also

[ps12image_overview_shiny_operator](https://github.com/tercen/ps12image_overview_shiny_operator)
[ps12image operator](https://github.com/tercen/ps12image_operator)

