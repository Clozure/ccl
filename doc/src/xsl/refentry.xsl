<?xml version='1.0' encoding="iso-8859-1"?>
<!DOCTYPE xsl:stylesheet [
  <!ENTITY lowercase "'abcdefghijklmnopqrstuvwxyz'">
  <!ENTITY uppercase "'ABCDEFGHIJKLMNOPQRSTUVWXYZ'">
]>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		version='1.0'
		xmlns="http://www.w3.org/TR/xhtml1/transitional"
		xmlns:ns="http://exslt.org/common"
		exclude-result-prefixes="#default">

  <!--
    <xsl:template name="refentry.list">
    <xsl:for-each select="refentry">
    <xsl:sort select="refnamediv/refname"/>
    
    <xsl:apply-templates select="."/>
    <xsl:apply-imports/>
    </xsl:for-each>
    </xsl:template>-->

  <xsl:template name="refentry.title">
    <xsl:param name="node" select="."/>
    
    <i><xsl:value-of select="$node/refnamediv/refclass"/></i>
    <xsl:text> </xsl:text>
    <b><xsl:value-of select="$node/refnamediv/refname"/></b>
  </xsl:template>

  <xsl:template match="refentry" mode="object.title.markup">
    <xsl:call-template name="refentry.title"/>
  </xsl:template>

  <xsl:template match="refentry" mode="title.markup">
    <xsl:call-template name="refentry.title"/>
  </xsl:template>

  <xsl:template match="refentry">
    <p>
      <div class="refentrytitle">
	<a>
	  <xsl:attribute name="id">
	    <xsl:value-of select="@id"/>
	  </xsl:attribute>
	</a>
	<strong>[<xsl:value-of select="refnamediv/refclass"/>]</strong><br/>
	<xsl:choose>
	  <xsl:when test="refsynopsisdiv/synopsis">
	    <code><xsl:apply-templates select="refsynopsisdiv/synopsis/node()"/></code>
	  </xsl:when>
	  <xsl:otherwise>
	    <code><xsl:value-of select="refnamediv/refname"/></code>
	  </xsl:otherwise>
	</xsl:choose>
      </div>
      <div class="refentrytitle">
	<xsl:value-of select="refnamediv/refpurpose"/>
      </div>
    </p>
    <p>
      <div>
	<xsl:apply-templates select="refsect1"/>
      </div>
    </p>
  </xsl:template>


  <xsl:template match="refentry" mode="xref-to">
    <b>
      <xsl:value-of
	 select="translate(refnamediv/refname[1], &uppercase;, &lowercase;)"/>
    </b>
  </xsl:template>

  <xsl:template match="refnamediv">
    <div class="refheader">
      <xsl:call-template name="refentry.title">
	<xsl:with-param name="node" select=".."/>
      </xsl:call-template>
    </div>
  </xsl:template>

  <xsl:template match="refsynopsisdiv">
    <div class="header">
      <xsl:text>Syntax:</xsl:text>
    </div>
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="synopsis">
    <div class="{name(.)}">
      <xsl:apply-templates select="function"/>
      <xsl:text> </xsl:text>
      <i>
	<xsl:apply-templates select="text()|*[position()>1]"/>
      </i>
    </div>
  </xsl:template>

  <xsl:template match="refsection|refsect1|refsect2|refsect3">
    <div class="{name(.)}">
      <xsl:call-template name="language.attribute"/>
      <xsl:call-template name="anchor">
	<xsl:with-param name="conditional" select="0"/>
      </xsl:call-template>
      <div class="header">
	<xsl:value-of select="title"/>
	<xsl:text>:</xsl:text>
      </div>
      <xsl:apply-templates select="text()|*[name() != 'title']"/>
    </div>
  </xsl:template>

  <xsl:template match="refsect1/variablelist">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="refsect1/variablelist/varlistentry">
    <p>
      <i>
	<xsl:apply-templates select="term"/>
      </i>
      <xsl:text>---</xsl:text>
      <xsl:apply-templates select="listitem"/>
    </p>
  </xsl:template>

  <xsl:template match="refsect1/variablelist/varlistentry/listitem">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="refsect1/variablelist/varlistentry/listitem/para">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="refsynopsisdiv/variablelist">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="refsynopsisdiv/variablelist/varlistentry">
    <p>
      <xsl:apply-templates select="term"/>
      <xsl:text> :: </xsl:text>
      <xsl:apply-templates select="listitem"/>
    </p>
  </xsl:template>

  <xsl:template match="refsynopsisdiv/variablelist/varlistentry/term">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="refsynopsisdiv/variablelist/varlistentry/listitem">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="refsynopsisdiv/variablelist/varlistentry/listitem/para">
    <xsl:apply-templates/>
  </xsl:template>
</xsl:stylesheet>
