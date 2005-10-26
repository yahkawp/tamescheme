<xsl:stylesheet version="1.0"
   xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="html"
              version="4.0"
              encoding="UTF-8"
              indent="yes"
              media-type="text/html"/>

  <!-- Parameters, site layout -->
  <xsl:param name="css">/style.css</xsl:param>
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

		  <a href="http://sourceforge.net"><img src="http://sourceforge.net/sflogo.php?group_id=151308&amp;type=1" width="88" height="31" border="0" alt="SourceForge.net Logo" /></a>
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
          <xsl:apply-templates select="descendant::topic[@name=$pagetopic]/../*" mode="menuchildren" />
        </ul>
      </xsl:if>
        
      <xsl:if test="@name=$pagetopic">
        <ul>
          <xsl:apply-templates select="topic" mode="menuchildren" />
        </ul>
      </xsl:if>
    </li>
  </xsl:template>

  <!-- Body text templates -->
  <xsl:template match="p">
    <p class="bodytext">
      <xsl:apply-templates/>
    </p>
  </xsl:template>
  
  <xsl:template match="note">
    <p class="note">
      <xsl:apply-templates/>
    </p>
  </xsl:template>

  <xsl:template match="b">
    <b>
      <xsl:apply-templates/>
    </b>
  </xsl:template>

  <xsl:template match="code">
    <code>
      <xsl:apply-templates/>
    </code>
  </xsl:template>

  <xsl:template match="class">
    <code class="class">
      <xsl:apply-templates/>
    </code>
  </xsl:template>

  <xsl:template match="interface">
    <code class="interface">
      <xsl:apply-templates/>
    </code>
  </xsl:template>

  <xsl:template match="method">
    <code class="method">
      <xsl:apply-templates/>
    </code>
  </xsl:template>
  
  <xsl:template match="enum">
    <code class="enumeration">
      <xsl:apply-templates/>
    </code>
  </xsl:template>
  
  <xsl:template match="variable">
    <code class="variable">
      <xsl:apply-templates/>
    </code>
  </xsl:template>

  <xsl:template match="scheme">
    <code class="scheme">
      <xsl:apply-templates/>
    </code>
  </xsl:template>

  <xsl:template match="i">
    <i>
      <xsl:apply-templates/>
    </i>
  </xsl:template>
  
  <xsl:template match="a">
    <a>
      <xsl:attribute name="href"><xsl:value-of select="@href"/></xsl:attribute>
      <xsl:apply-templates/>
    </a>
  </xsl:template>
  
  <xsl:template match="ul">
    <ul>
      <xsl:apply-templates/>
    </ul>
  </xsl:template>
  
  <xsl:template match="li">
    <li>
      <xsl:apply-templates/>
    </li>
  </xsl:template>

  <!-- Headers -->
  <xsl:template match="h1">
    <h1>
      <xsl:apply-templates/>
    </h1>
  </xsl:template>

  <xsl:template match="h2">
    <h2>
      <xsl:apply-templates/>
    </h2>
  </xsl:template>

  <xsl:template match="h3">
    <h3>
      <xsl:apply-templates/>
    </h3>
  </xsl:template>

  <xsl:template match="dl">
    <dl>
      <xsl:apply-templates/>
    </dl>
  </xsl:template>

  <xsl:template match="dd">
    <dd>
      <xsl:apply-templates/>
    </dd>
  </xsl:template>

  <xsl:template match="dt">
    <dt>
      <xsl:apply-templates/>
    </dt>
  </xsl:template>

  <xsl:template match="img">
    <img>
      <xsl:attribute name="src"><xsl:value-of select="@src"/></xsl:attribute>
      <xsl:attribute name="alt"><xsl:value-of select="@alt"/></xsl:attribute>
    </img>
  </xsl:template>

  <xsl:template match="br">
    <br />
  </xsl:template>
  
  <!-- A few 'special' features -->
  <xsl:template match="section">
    <!-- A section is basically a prettier headline -->
    <p class="section">
      <xsl:apply-templates/>
    </p>
  </xsl:template>
  
  <xsl:template match="sideimg">
    <div class="sideimg">
      <a>
        <xsl:attribute name="href"><xsl:value-of select="@link" /></xsl:attribute>
        <img>
          <xsl:attribute name="src"><xsl:value-of select="@src" /></xsl:attribute>
        </img>
      </a>
    </div>
  </xsl:template>
  
  <xsl:template match="item">
    <div class="item">
      <table class="title" width="100%" cellpadding="0" cellspacing="0">
        <tr>
        <td class="theTitle">
          <xsl:choose>
            <xsl:when test="@url">
              <a>
                <xsl:attribute name="href"><xsl:value-of select="@url"/></xsl:attribute>
                <xsl:value-of select="@title"/>
              </a>
            </xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="@title"/>
            </xsl:otherwise>
          </xsl:choose>
        </td>
        <td class="theDate" align="right">
          <xsl:value-of select="@date"/>
        </td>
        </tr>
      </table>
      <div class="itemcontent">
        <xsl:apply-templates/>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="*">
    <xsl:apply-templates/>
  </xsl:template>
</xsl:stylesheet>
