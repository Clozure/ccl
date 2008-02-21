<?xml version="1.0" encoding="iso-8859-1"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		version='1.0'
		xmlns="http://www.w3.org/TR/xhtml1/transitional"
		exclude-result-prefixes="#default">
  <xsl:param name="local.l10n.xml" select="document('')"/>

  <l:i18n xmlns:l="http://docbook.sourceforge.net/xmlns/l10n/1.0">
    <l:l10n language="en">
      <l:gentext key="nav-home" text="Table of Contents"/>
    </l:l10n>
  </l:i18n>

  <xsl:template match="varname">
    <xsl:call-template name="inline.italicseq"/>
  </xsl:template>

  <xsl:template match="function">
    <xsl:call-template name="inline.boldseq"/>
  </xsl:template>

  <xsl:template match="type">
    <xsl:call-template name="inline.boldseq"/>
  </xsl:template>
 
  <xsl:template match="property">
    <xsl:call-template name="inline.boldseq"/>
  </xsl:template>
</xsl:stylesheet>
