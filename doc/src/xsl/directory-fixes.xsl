<?xml version="1.0" encoding="iso-8859-1"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		version='1.0'
		xmlns="http://www.w3.org/TR/xhtml1/transitional"
		exclude-result-prefixes="#default">

  <xsl:param name="openmcl.depth">
    <xsl:choose>
      <xsl:when test="$openmcl.directory = './'">
	<xsl:value-of select="0"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:call-template name="count.uri.path.depth">
	  <xsl:with-param name="filename" select="$openmcl.directory"/>
	</xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:param>

  <xsl:param name="openmcl.base">
    <xsl:call-template name="copy-string">
      <xsl:with-param name="string" select="'../'"/>
      <xsl:with-param name="count" select="$openmcl.depth"/>
    </xsl:call-template>
  </xsl:param>

  <xsl:param name="html.stylesheet">
    <xsl:value-of select="$openmcl.base"/>
    <xsl:text>openmcl.css</xsl:text>
  </xsl:param>

  <!-- Be aware that href.target.uri cannot be defined in this file,
       because it would be overridden by the definition in
       optional-onechunk.xsl. -->

</xsl:stylesheet>
