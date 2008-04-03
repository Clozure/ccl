<?xml version="1.0" encoding="iso-8859-1"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		version='1.0'
		xmlns="http://www.w3.org/TR/xhtml1/transitional"
		xmlns:date="http://exslt.org/dates-and-times"
		exclude-result-prefixes="#default">

  <xsl:import href="http://docbook.sourceforge.net/release/xsl/current/xhtml/docbook.xsl"/>
  <xsl:import href="http://docbook.sourceforge.net/release/xsl/current/xhtml/chunk-common.xsl"/>

  <xsl:import href="toc-at-end.xsl"/>

  <xsl:include href="http://docbook.sourceforge.net/release/xsl/current/html/manifest.xsl"/>

  <xsl:include href="parameters.xsl"/>
  <!-- xsl:include href="site-navigator.xsl"/ -->
  <xsl:include href="footer.xsl"/>
  <xsl:include href="minor-customizations.xsl"/>
  <xsl:include href="optional-onechunk.xsl"/>

  <xsl:include href="http://docbook.sourceforge.net/release/xsl/current/html/chunk-code.xsl"/>
  <xsl:include href="refentry.xsl"/>
</xsl:stylesheet>