<?xml version='1.0' encoding="iso-8859-1"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		version='1.0'
		xmlns="http://www.w3.org/TR/xhtml1/transitional"
		exclude-result-prefixes="#default">

  <xsl:param name="section.autolabel" select="1"/>
  <xsl:param name="section.label.includes.component.label" select="1"/>
  
  <xsl:variable name="toc.max.depth">2</xsl:variable>
  
  <xsl:param name="html.extra.head.links" select="0"/>

  <xsl:param name="chunk.first.sections" select="1"/>
  <xsl:param name="chunk.section.depth" select="1"/>
  
  <xsl:param name="chunk.fast" select="1"/>
  <xsl:param name="chunker.output.indent" select="'yes'"/>

  <xsl:param name="generate.toc">
    appendix  toc
    article/appendix  nop
    article   toc
    book      toc,figure,table,example,equation
    chapter   toc
    part      toc
    preface   toc
    qandadiv  toc
    qandaset  toc
    reference toc
    sect1     toc
    sect2     toc
    sect3     toc
    sect4     toc
    sect5     toc
    section   toc
    set       toc
  </xsl:param>
</xsl:stylesheet>