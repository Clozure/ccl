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

  <xsl:param name="suppress.navigation">
    <xsl:choose>
      <xsl:when test="$onechunk != 0">
	<xsl:value-of select="1"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="0"/>
      </xsl:otherwise>    
    </xsl:choose>
  </xsl:param>

  <xsl:template name="href.target.uri">
    <xsl:param name="object" select="."/>
    <xsl:choose>
      <xsl:when test="$onechunk != 0">
	<!-- The contents of this block are entirely taken from
	     onechunk.xsl in the Norman Walsh stylesheets, version 1.65.1. -->
	<xsl:text>#</xsl:text>
	<xsl:call-template name="object.id">
	  <xsl:with-param name="object" select="$object"/>
	</xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
	<!-- The contents of this block are entirely taken from
             chunk-common.xsl in the Norman Walsh stylesheets,
             version 1.65.1. -->
	<xsl:variable name="ischunk">
	  <xsl:call-template name="chunk">
	    <xsl:with-param name="node" select="$object"/>
	  </xsl:call-template>
	</xsl:variable>
	
	<xsl:apply-templates mode="chunk-filename" select="$object"/>
	
	<xsl:if test="$ischunk='0'">
	  <xsl:text>#</xsl:text>
	  <xsl:call-template name="object.id">
	    <xsl:with-param name="object" select="$object"/>
	  </xsl:call-template>
	</xsl:if>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
</xsl:stylesheet>
