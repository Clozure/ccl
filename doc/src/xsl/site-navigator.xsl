<?xml version="1.0" encoding="iso-8859-1"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		version='1.0'
		xmlns="http://www.w3.org/TR/xhtml1/transitional"
		exclude-result-prefixes="#default">

  <xsl:template name="user.header.navigation">
    <xsl:element name="table">
      <xsl:attribute name="width">100%</xsl:attribute>
      <xsl:attribute name="border">0</xsl:attribute>
      <xsl:attribute name="cellspacing">0</xsl:attribute>
      <xsl:attribute name="cellpadding">0</xsl:attribute>
      <tr>
	<td colspan="2" align="center">
	  <hr/>
	  <center>
	    <font face="arial" size="2">
	      [
	      <xsl:element name="a">
		<xsl:attribute name="href"><xsl:value-of select="$openmcl.base"/>index.html</xsl:attribute>
		Home
	      </xsl:element>
	      |
	      <xsl:element name="a">
		<xsl:attribute name="href"><xsl:value-of select="$openmcl.base"/>FAQ</xsl:attribute>
		FAQ
	      </xsl:element>
	      |
	      <xsl:element name="a">
		<xsl:attribute name="href"><xsl:value-of select="$openmcl.base"/>Doc/index.html</xsl:attribute>
		Documentation
	      </xsl:element>
	      |
	      <xsl:element name="a">
		<xsl:attribute name="href"><xsl:value-of select="$openmcl.base"/>Distributions/index.html</xsl:attribute>
		Distributions
	      </xsl:element>
	      |
	      <xsl:element name="a">
		<xsl:attribute name="href"><xsl:value-of select="$openmcl.base"/>TmpCVS/index.html</xsl:attribute>
		CVS Access
	      </xsl:element>
	      |
	      <xsl:element name="a">
		<xsl:attribute name="href"><xsl:value-of select="$openmcl.base"/>mail/index.html</xsl:attribute>
		Mailing-Lists
	      </xsl:element>
	      |
	      <xsl:element name="a">
		<xsl:attribute name="href"><xsl:value-of select="$openmcl.base"/>support.html</xsl:attribute>
		Support
	      </xsl:element>
	      |
	      <a href="http://openmcl.clozure.com/openmcl-wiki">Wiki</a>
	      ]
	    </font>
	    <hr/>
	  </center>
	</td>
      </tr>
    </xsl:element>
  </xsl:template>
</xsl:stylesheet>
