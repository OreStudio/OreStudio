/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_QT_ORG_DOC_RENDERER_HPP
#define ORES_QT_ORG_DOC_RENDERER_HPP

#include "ores.orgmode/domain/document.hpp"
#include "ores.qt.headless/export.hpp"
#include <QString>
#include <string>
#include <vector>

namespace ores::qt {

/**
 * @brief Render a parsed org doc (from =ores.orgmode=) into rich-text
 * HTML suitable for a =QTextBrowser=.
 *
 * Deliberately not a faithful org renderer — headings, bullet lists,
 * tables, and the four common inline markers org-mode itself uses
 * (=*bold*=, =/italic/=, ===code==, =~verbatim~=) are enough to make a
 * story/task/scenario doc readable without leaving the app. Anything
 * else in the body (source blocks, footnotes, ...) is shown as plain
 * text, not stripped or mis-rendered.
 *
 * Shared by the story/task context viewer and the QA Validation
 * Runner panel — one renderer, not two.
 */
ORES_QT_HEADLESS_API QString render_org_doc_to_html(const orgmode::domain::document& doc);

/**
 * @brief Render a single heading (and its children, recursively) —
 * used when only part of a document needs showing.
 */
ORES_QT_HEADLESS_API QString render_heading_to_html(const orgmode::domain::heading& heading);

/**
 * @brief Render a heading's raw =body_lines= (paragraphs and bullet
 * lists, wrapped-line-joined and inline-marked-up the same way
 * =render_heading_to_html= renders its own body) without the heading
 * itself — used for the QA Validation Runner's step detail view, which
 * shows a step's instructional text but not a redundant heading title
 * (already shown as the dialog's own title).
 */
ORES_QT_HEADLESS_API QString render_body_lines_to_html(const std::vector<std::string>& lines);

}

#endif
