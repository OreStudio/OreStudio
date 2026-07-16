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
#ifndef ORES_MARKETDATA_CLIENT_PRESENTATION_CRM_RATE_TABLE_IO_HPP
#define ORES_MARKETDATA_CLIENT_PRESENTATION_CRM_RATE_TABLE_IO_HPP

#include "ores.marketdata.client/export.hpp"
#include "ores.marketdata.client/presentation/crm_rate_display_service.hpp"
#include <iosfwd>
#include <vector>

namespace ores::marketdata::client::presentation {

/**
 * @brief Dumps the CRM rate rows to a stream in table format.
 */
ORES_MARKETDATA_CLIENT_EXPORT std::ostream&
operator<<(std::ostream& s, const std::vector<crm_rate_display_service::row>& v);

}

#endif
