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
#ifndef ORES_ANALYTICS_CORE_VALIDATION_PRICING_ENGINES_DOCUMENT_VALIDATION_HPP
#define ORES_ANALYTICS_CORE_VALIDATION_PRICING_ENGINES_DOCUMENT_VALIDATION_HPP

#include "ores.analytics.api/messaging/configuration_document_protocol.hpp"
#include "ores.analytics.core/export.hpp"
#include <vector>

namespace ores::analytics::validation {

/**
 * @brief Every problem that stops this document from being stored.
 *
 * A document is written whole, so a document ORE cannot run is refused before
 * any row is written rather than stored and repaired later. All of the problems
 * come back, not the first, so a person fixes them in one pass: the
 * configuration's own first, then the products, then their settings, in the
 * order the document states them.
 *
 * A document that passes returns nothing.
 */
[[nodiscard]] ORES_ANALYTICS_CORE_EXPORT std::vector<messaging::pricing_engines_document_problem>
problems_of(const messaging::pricing_engines_document& document);

}

#endif
