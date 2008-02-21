<?xml version="1.0" encoding="iso-8859-1"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		version='1.0'
		xmlns="http://www.w3.org/TR/xhtml1/transitional"
		exclude-result-prefixes="#default">

  <!-- Based on Norman Walsh's stylesheets, version 1.62.4.  Last updated
       in June 2004. -->

  <!-- From component.xsl. -->
  <xsl:template match="preface">
    <div class="{name(.)}">
      <xsl:call-template name="language.attribute"/>
      <xsl:if test="$generate.id.attributes != 0">
	<xsl:attribute name="id">
	  <xsl:call-template name="object.id"/>
	</xsl:attribute>
      </xsl:if>

      <xsl:call-template name="component.separator"/>
      <xsl:call-template name="preface.titlepage"/>

      <xsl:variable name="toc.params">
	<xsl:call-template name="find.path.params">
	  <xsl:with-param name="table" select="normalize-space($generate.toc)"/>
	</xsl:call-template>
      </xsl:variable>

      <xsl:apply-templates/>
      <xsl:call-template name="process.footnotes"/>

      <xsl:if test="contains($toc.params, 'toc')">
	<xsl:call-template name="component.toc.separator"/>
	<xsl:call-template name="component.toc">
	  <xsl:with-param name="toc.title.p" select="contains($toc.params, 'title')"/>
	</xsl:call-template>
      </xsl:if>
    </div>
  </xsl:template>

  <!-- From component.xsl. -->
  <xsl:template match="chapter">
    <div class="{name(.)}">
      <xsl:call-template name="language.attribute"/>
      <xsl:if test="$generate.id.attributes != 0">
	<xsl:attribute name="id">
	  <xsl:call-template name="object.id"/>
	</xsl:attribute>
      </xsl:if>

      <xsl:call-template name="component.separator"/>
      <xsl:call-template name="chapter.titlepage"/>

      <xsl:variable name="toc.params">
	<xsl:call-template name="find.path.params">
	  <xsl:with-param name="table" select="normalize-space($generate.toc)"/>
	</xsl:call-template>
      </xsl:variable>

      <xsl:apply-templates/>
      <xsl:call-template name="process.footnotes"/>

      <xsl:if test="contains($toc.params, 'toc')">
	<xsl:call-template name="component.toc.separator"/>
	<xsl:call-template name="component.toc">
	  <xsl:with-param name="toc.title.p" select="contains($toc.params, 'title')"/>
	</xsl:call-template>
      </xsl:if>
    </div>
  </xsl:template>

  <!-- From component.xsl. -->
  <xsl:template match="appendix">
    <xsl:variable name="ischunk">
      <xsl:call-template name="chunk"/>
    </xsl:variable>

    <div class="{name(.)}">
      <xsl:call-template name="language.attribute"/>
      <xsl:if test="$generate.id.attributes != 0">
	<xsl:attribute name="id">
	  <xsl:call-template name="object.id"/>
	</xsl:attribute>
      </xsl:if>

      <xsl:choose>
	<xsl:when test="parent::article and $ischunk = 0">
	  <xsl:call-template name="section.heading">
	    <xsl:with-param name="level" select="1"/>
	    <xsl:with-param name="title">
	      <xsl:apply-templates select="." mode="object.title.markup"/>
	    </xsl:with-param>
	  </xsl:call-template>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:call-template name="component.separator"/>
	  <xsl:call-template name="appendix.titlepage"/>
	</xsl:otherwise>
      </xsl:choose>

      <xsl:variable name="toc.params">
	<xsl:call-template name="find.path.params">
	  <xsl:with-param name="table" select="normalize-space($generate.toc)"/>
	</xsl:call-template>
      </xsl:variable>

      <xsl:apply-templates/>

      <xsl:if test="not(parent::article) or $ischunk != 0">
	<xsl:call-template name="process.footnotes"/>
      </xsl:if>

      <xsl:if test="contains($toc.params, 'toc')">
	<xsl:call-template name="component.toc.separator"/>
	<xsl:call-template name="component.toc">
	  <xsl:with-param name="toc.title.p" select="contains($toc.params, 'title')"/>
	</xsl:call-template>
      </xsl:if>
    </div>
  </xsl:template>

  <!-- From component.xsl.  Commented out because I actually prefer to leave
       the ToC at the beginning for articles. -->
<!--
  <xsl:template match="article">
    <div class="{name(.)}">
      <xsl:call-template name="language.attribute"/>
      <xsl:if test="$generate.id.attributes != 0">
	<xsl:attribute name="id">
	  <xsl:call-template name="object.id"/>
	</xsl:attribute>
      </xsl:if>

      <xsl:call-template name="article.titlepage"/>

      <xsl:variable name="toc.params">
	<xsl:call-template name="find.path.params">
	  <xsl:with-param name="table" select="normalize-space($generate.toc)"/>
	</xsl:call-template>
      </xsl:variable>

      <xsl:apply-templates/>
      <xsl:call-template name="process.footnotes"/>

      <xsl:call-template name="make.lots">
	<xsl:with-param name="toc.params" select="$toc.params"/>
	<xsl:with-param name="toc">
	  <xsl:call-template name="component.toc">
	    <xsl:with-param name="toc.title.p" select="contains($toc.params, 'title')"/>
	  </xsl:call-template>
	</xsl:with-param>
      </xsl:call-template>
    </div>
  </xsl:template>
-->

  <!-- From section.xsl. -->
  <xsl:template match="section">
    <xsl:variable name="depth" select="count(ancestor::section)+1"/>

    <div class="{name(.)}">
      <xsl:call-template name="language.attribute"/>
      <xsl:call-template name="section.titlepage"/>

      <xsl:variable name="toc.params">
	<xsl:call-template name="find.path.params">
	  <xsl:with-param name="table" select="normalize-space($generate.toc)"/>
	</xsl:call-template>
      </xsl:variable>

      <xsl:apply-templates/>
      <xsl:call-template name="process.chunk.footnotes"/>

      <xsl:if test="contains($toc.params, 'toc')
	      and $depth &lt;= $generate.section.toc.level">
	<xsl:call-template name="section.toc.separator"/>
	<xsl:call-template name="section.toc">
	  <xsl:with-param name="toc.title.p" select="contains($toc.params, 'title')"/>
	</xsl:call-template>
      </xsl:if>
    </div>
  </xsl:template>

  <!-- From section.xsl. -->
  <xsl:template match="sect1">
    <div class="{name(.)}">
      <xsl:call-template name="language.attribute"/>
      <xsl:call-template name="sect1.titlepage"/>

      <xsl:variable name="toc.params">
	<xsl:call-template name="find.path.params">
	  <xsl:with-param name="table" select="normalize-space($generate.toc)"/>
	</xsl:call-template>
      </xsl:variable>

      <xsl:apply-templates/>
      <xsl:call-template name="process.chunk.footnotes"/>

      <xsl:if test="contains($toc.params, 'toc')
	      and $generate.section.toc.level &gt;= 1">
	<xsl:call-template name="section.toc.separator"/>
	<xsl:call-template name="section.toc">
	  <xsl:with-param name="toc.title.p" select="contains($toc.params, 'title')"/>
	</xsl:call-template>
      </xsl:if>
    </div>
  </xsl:template>

  <!-- From section.xsl. -->
  <xsl:template match="sect2">
    <div class="{name(.)}">
      <xsl:call-template name="language.attribute"/>
      <xsl:call-template name="sect2.titlepage"/>

      <xsl:variable name="toc.params">
	<xsl:call-template name="find.path.params">
	  <xsl:with-param name="table" select="normalize-space($generate.toc)"/>
	</xsl:call-template>
      </xsl:variable>

      <xsl:apply-templates/>
      <xsl:call-template name="process.chunk.footnotes"/>

      <xsl:if test="contains($toc.params, 'toc')
	      and $generate.section.toc.level &gt;= 2">
	<xsl:call-template name="section.toc.separator"/>
	<xsl:call-template name="section.toc">
	  <xsl:with-param name="toc.title.p" select="contains($toc.params, 'title')"/>
	</xsl:call-template>
      </xsl:if>
    </div>
  </xsl:template>

  <!-- From section.xsl. -->
  <xsl:template match="sect3">
    <div class="{name(.)}">
      <xsl:call-template name="language.attribute"/>
      <xsl:call-template name="sect3.titlepage"/>

      <xsl:variable name="toc.params">
	<xsl:call-template name="find.path.params">
	  <xsl:with-param name="table" select="normalize-space($generate.toc)"/>
	</xsl:call-template>
      </xsl:variable>

      <xsl:apply-templates/>
      <xsl:call-template name="process.chunk.footnotes"/>

      <xsl:if test="contains($toc.params, 'toc')
	      and $generate.section.toc.level &gt;= 3">
	<xsl:call-template name="section.toc.separator"/>
	<xsl:call-template name="section.toc">
	  <xsl:with-param name="toc.title.p" select="contains($toc.params, 'title')"/>
	</xsl:call-template>
      </xsl:if>
    </div>
  </xsl:template>

  <!-- From section.xsl. -->
  <xsl:template match="sect4">
    <div class="{name(.)}">
      <xsl:call-template name="language.attribute"/>
      <xsl:call-template name="sect4.titlepage"/>

      <xsl:variable name="toc.params">
	<xsl:call-template name="find.path.params">
	  <xsl:with-param name="table" select="normalize-space($generate.toc)"/>
	</xsl:call-template>
      </xsl:variable>

      <xsl:apply-templates/>
      <xsl:call-template name="process.chunk.footnotes"/>

      <xsl:if test="contains($toc.params, 'toc')
	      and $generate.section.toc.level &gt;= 4">
	<xsl:call-template name="section.toc.separator"/>
	<xsl:call-template name="section.toc">
	  <xsl:with-param name="toc.title.p" select="contains($toc.params, 'title')"/>
	</xsl:call-template>
      </xsl:if>
    </div>
  </xsl:template>

  <!-- From section.xsl. -->
  <xsl:template match="sect5">
    <div class="{name(.)}">
      <xsl:call-template name="language.attribute"/>
      <xsl:call-template name="sect5.titlepage"/>

      <xsl:variable name="toc.params">
	<xsl:call-template name="find.path.params">
	  <xsl:with-param name="table" select="normalize-space($generate.toc)"/>
	</xsl:call-template>
      </xsl:variable>

      <xsl:apply-templates/>
      <xsl:call-template name="process.chunk.footnotes"/>

      <xsl:if test="contains($toc.params, 'toc')
	      and $generate.section.toc.level &gt;= 5">
	<xsl:call-template name="section.toc.separator"/>
	<xsl:call-template name="section.toc">
	  <xsl:with-param name="toc.title.p" select="contains($toc.params, 'title')"/>
	</xsl:call-template>
      </xsl:if>
    </div>
  </xsl:template>
</xsl:stylesheet>