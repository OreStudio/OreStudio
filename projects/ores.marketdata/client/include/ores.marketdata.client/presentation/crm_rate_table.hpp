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
#ifndef ORES_MARKETDATA_CLIENT_PRESENTATION_CRM_RATE_TABLE_HPP
#define ORES_MARKETDATA_CLIENT_PRESENTATION_CRM_RATE_TABLE_HPP

#include "ores.marketdata.client/export.hpp"
#include "ores.marketdata.client/presentation/crm_rate_display_service.hpp"
#include <string>
#include <vector>

namespace ores::marketdata::client::presentation {

/**
 * @brief Converts already-formatted CRM rate rows to the table format --
 * the same rate/change/tooltip text a Qt cell shows, laid out as one row
 * per (base, quote), for any text-console consumer (ores.shell today).
 */
ORES_MARKETDATA_CLIENT_EXPORT std::string
convert_to_table(const std::vector<crm_rate_display_service::row>& v);

/**
 * @brief Converts already-formatted CRM rate rows to an NxN grid --
 * base currencies as rows, quote currencies as columns, each cell the
 * formatted rate text -- mirroring the Qt Cross-Rates Matrix window's
 * own layout for a text-console consumer (ores.shell's --matrix flag).
 * Only meaningful for a single CRM's rows: mixing rows from several
 * enabled CRMs would silently overwrite a (base, quote) cell with
 * whichever CRM's row happens to be seen last.
 */
ORES_MARKETDATA_CLIENT_EXPORT std::string
convert_to_matrix_table(const std::vector<crm_rate_display_service::row>& v);

}

#endif
