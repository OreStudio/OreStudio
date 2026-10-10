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
#ifndef ORES_REFDATA_SERVICE_MESSAGING_BOOK_PROPOSAL_MAPPING_HPP
#define ORES_REFDATA_SERVICE_MESSAGING_BOOK_PROPOSAL_MAPPING_HPP

#include "ores.refdata.api/domain/book_change.hpp"
#include "ores.refdata.api/messaging/book_proposal_operations_protocol.hpp"
#include "ores.refdata.service/export.hpp"
#include "ores.refdata.service/service/book_proposal.hpp"
#include <string>
#include <vector>

namespace ores::refdata::messaging {

/**
 * @brief The permissions a caller needs to propose the lines.
 *
 * A proposal asks for a write, so the caller needs the permission the write
 * itself needs: books:delete for a delete line and books:write for any other.
 * Each permission is named once, in a fixed order.
 */
ORES_REFDATA_SERVICE_EXPORT std::vector<std::string>
required_permissions(const std::vector<domain::book_change>& lines);

/**
 * @brief The outcome of each line as the wire states it.
 */
ORES_REFDATA_SERVICE_EXPORT std::vector<book_line_outcome>
to_outcomes(const service::book_preview& preview);

}

#endif
