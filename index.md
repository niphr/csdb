---
title: csdb
---

<p class="cs-section">Features</p>

<div class="cs-cards">
<div class="cs-card"><div class="cs-card-num">01</div><h3>Connection management</h3><p><code>DBConnection_v9</code> wraps an ODBC connection with lazy connect, explicit disconnect, and automatic reconnect via <code>$autoconnection</code>.</p></div>
<div class="cs-card"><div class="cs-card-num">02</div><h3>Bulk table operations</h3><p><code>DBTable_v9</code> handles insert, upsert, and key-based row deletion against PostgreSQL and SQL Server tables, with configurable indexes.</p></div>
<div class="cs-card"><div class="cs-card-num">03</div><h3>Field validation</h3><p>Pluggable validators check field types and field contents before data reaches the database, with built-in schemas for the csverse <code>csfmt_rts_data</code> format.</p></div>
</div>

## Overview 

[csdb](https://niphr.github.io/csdb/) provides an abstracted system for easily working with databases with large datasets.

Read the introduction vignette [here](https://niphr.github.io/csdb/articles/csdb.html) or run `help(package="csdb")`.
