<xsl:stylesheet version="1.0"
   xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="html"
              version="4.0"
              encoding="UTF-8"
              indent="yes"
              media-type="text/html"/>

  <!-- Parameters, site layout -->
  <xsl:param name="css">style.css</xsl:param>
  <xsl:param name="imgs">imgs</xsl:param>
  <xsl:variable name="topics" select="document(/page/@layout)" />
  <xsl:variable name="pagetopic"><xsl:value-of select="/page/@topic" /></xsl:variable>

  <!-- The document itself -->
  <xsl:template match="page">
    <html>
      <head>
        <title><xsl:value-of select="/page/title"/></title>
        <link rel="stylesheet" type="text/css">
        	<xsl:attribute name="href"><xsl:value-of select="$css"/></xsl:attribute>
        </link>
      </head>
      
      <!-- New layout -->
      
      <body>
	    <div class="heading">
		  <p class="logo"><img src="/imgs/logo.gif" /></p>
		  <ul>
       	    <xsl:apply-templates select="$topics/menu/topic" mode="topmenu" />
       	  </ul>
		</div>
	
	    <div class="sidebar">
       	  <xsl:apply-templates select="$topics/menu/topic[descendant-or-self::topic[@name=$pagetopic]]" mode="menutopics" />
	    </div>

		<div class="bodytext">
		  <div class="bodytext_tl">
			<div class="bodytext_tr">
			  <div class="bodytext_t">
			  </div>
			</div>
		  </div>
			
			<div class="bodytext_l">
			  <div class="bodytext_r">
				<div class="bodytext_middle">
				  <xsl:apply-templates select="text/*"/>
				</div>
			  </div>
			</div>
	
			<div class="bodytext_bl">
			  <div class="bodytext_br">
				<div class="bodytext_b">
				</div>
			  </div>
			</div>
		</div>
      </body>
    </html>
  </xsl:template>

  <!-- Menu topics -->
  <xsl:template match="topic" mode="topmenu">
    <li>
      (<a>
        <xsl:if test="descendant-or-self::topic[@name=$pagetopic]">
          <xsl:attribute name="class">selected</xsl:attribute>
        </xsl:if>

        <xsl:attribute name="href"><xsl:value-of select="@url" /></xsl:attribute>
        <xsl:value-of select="@name" />    
      </a>)
    </li>
  </xsl:template>
  
  <xsl:template match="topic" mode="menutopics">
    <p class="topic"><xsl:value-of select="@name" /></p>
    
    <ul>
      <xsl:apply-templates select="topic" mode="menuchildren" />
    </ul>
  </xsl:template>
    
  <xsl:template match="topic" mode="menuchildren">
    <li>&#xBB;
      <a>
        <xsl:if test="descendant-or-self::topic[@name=$pagetopic]">
          <xsl:attribute name="class">selected</xsl:attribute>
        </xsl:if>

        <xsl:attribute name="href"><xsl:value-of select="@url" /></xsl:attribute>
        <xsl:value-of select="@name" />
      </a>
      
      <xsl:if test="descendant::topic[@name=$pagetopic]">
        <ul>
          <xsl:apply-templates select="descendant::topic[@name=$pagetopic]" mode="menuchildren" />
        </ul>
      </xsl:if>
        
      <xsl:if test="@name=$pagetopic">
        <ul>
          <xsl:apply-templates select="topic" mode="menuchildren" />
        </ul>
      </xsl:if>
    </li>
  </xsl:template>

</xsl:stylesheet>
