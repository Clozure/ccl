<?xml version="1.0" encoding="iso-8859-1"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		version='1.0'
		xmlns="http://www.w3.org/TR/xhtml1/transitional"
		xmlns:date="http://exslt.org/dates-and-times"
		exclude-result-prefixes="#default">

  <xsl:template name="user.footer.navigation">
    <xsl:if test="count(/book/index) > 0">
      <div align="center">
	<a>
	  <xsl:attribute name="href">
	    <xsl:call-template name="href.target">
	      <xsl:with-param name="object" select="/book/index"/>
	    </xsl:call-template>
	  </xsl:attribute>
	  <xsl:text>Symbol Index</xsl:text>
	</a>
      </div>
    </xsl:if>

    <p class="footer">
      <xsl:variable name="now" select="date:date-time()"/>
      <xsl:text>This document was last modified at </xsl:text>
      <xsl:value-of select="date:hour-in-day($now)"/>
      <xsl:text>:</xsl:text>
      <xsl:value-of select="date:minute-in-hour($now)"/>     
      <xsl:text> on </xsl:text>
      <xsl:value-of select="date:month-name($now)"/>
      <xsl:text> </xsl:text>
      <xsl:value-of select="date:day-in-month($now)"/>
      <xsl:text>, </xsl:text>
      <xsl:value-of select="date:year($now)"/>
      <xsl:text>, in UTC.</xsl:text>
      <br/>
      <xsl:text>It uses version </xsl:text>
      <xsl:value-of select="$VERSION"/>
      <xsl:text> of the Norman Walsh Docbook stylesheets.</xsl:text>
      <br/>
      <xsl:text>Built from subversion rev </xsl:text>
      <xsl:value-of select="$svnrev"/>
      <br/>
      <xsl:value-of select="$xsltproc.version"/>
    </p>
  </xsl:template>
</xsl:stylesheet>
