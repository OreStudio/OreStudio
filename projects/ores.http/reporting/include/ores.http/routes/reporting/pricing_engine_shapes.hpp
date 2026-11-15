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
#ifndef ORES_HTTP_ROUTES_REPORTING_PRICING_ENGINE_SHAPES_HPP
#define ORES_HTTP_ROUTES_REPORTING_PRICING_ENGINE_SHAPES_HPP

#include "ores.analytics.api/domain/pricing_model_config.hpp"
#include "ores.http/routes/reporting/engine_settings_rows.hpp"
#include "ores.http/routes/reporting/pricing_engine_rows.hpp"
#include <map>
#include <string>
#include <string_view>
#include <vector>

namespace ores::http::routes::reporting::pricing_engine_shapes {

/**
 * @brief The party a configuration belongs to, and the names of its parties.
 */
[[nodiscard]] std::vector<std::string>
party_members(std::string_view party_id, const std::map<std::string, engine_party_name>& parties);

/**
 * @brief One configuration as the page of engines states it.
 */
[[nodiscard]] std::string summary_json_of(const ores::analytics::domain::pricing_model_config& row,
                                          const pricing_engine_rows::document_counts& counts,
                                          const std::map<std::string, engine_party_name>& parties);

/**
 * @brief One configuration with its products, its parameters and the reports
 * that bind it.
 *
 * A read answers a document and a write answers the document it wrote, so the
 * shape is stated once here rather than twice: a create that answered another
 * shape would be a second statement of the screen's contract.
 *
 * @param reports The reports as JSON, or =null= when they could not be read.
 */
[[nodiscard]] std::string document_json_of(const pricing_document_row& document,
                                           const std::map<std::string, engine_party_name>& parties,
                                           const std::string& reports);

}

#endif
