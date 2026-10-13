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
#ifndef ORES_REFDATA_SERVICE_MESSAGING_COUNTERPARTY_PROPOSAL_MAPPING_HPP
#define ORES_REFDATA_SERVICE_MESSAGING_COUNTERPARTY_PROPOSAL_MAPPING_HPP

#include "ores.refdata.api/messaging/counterparty_proposal_operations_protocol.hpp"
#include "ores.refdata.service/export.hpp"
#include "ores.refdata.service/messaging/book_proposal_mapping.hpp"
#include "ores.refdata.service/service/counterparty_proposal.hpp"
#include <string>
#include <vector>

namespace ores::refdata::messaging {

/**
 * @brief Why the lines cannot be proposed at all, or empty when they can.
 *
 * There must be at least one line, and each line must be a put or a delete. The
 * permission check reads the operation, so a value it does not know is refused
 * before the check and never counted as a write.
 */
ORES_REFDATA_SERVICE_EXPORT std::string
invalid_lines(const service::counterparty_lines& lines);

/**
 * @brief The permissions a caller needs to propose the lines.
 *
 * A proposal asks for a write, so the caller needs the permission each write
 * itself needs: the write permission of a kind for its put lines and its delete
 * permission for its delete lines. Each permission is named once, in a fixed
 * order. The lines have passed invalid_lines.
 */
ORES_REFDATA_SERVICE_EXPORT std::vector<std::string>
required_permissions(const service::counterparty_lines& lines);

/**
 * @brief The re-check step's end: completed when no line is refused, and
 * otherwise failed with the first refused line and its reason.
 */
ORES_REFDATA_SERVICE_EXPORT step_result
recheck_step_result(const service::counterparty_preview& preview);

/**
 * @brief The apply step's end.
 *
 * Completed when every line applied. A refusal is failed with the line and its
 * reason, and the request has moved to apply_failed. A failure of the
 * infrastructure is failed with a reason that says the apply may run again.
 */
ORES_REFDATA_SERVICE_EXPORT step_result
apply_step_result(const service::counterparty_apply& applied);

/**
 * @brief The outcome of each line as the wire states it.
 */
ORES_REFDATA_SERVICE_EXPORT std::vector<counterparty_line_outcome>
to_outcomes(const service::counterparty_preview& preview);

/**
 * @brief The stored lines as the wire states them.
 */
ORES_REFDATA_SERVICE_EXPORT std::vector<counterparty_change_delta>
to_deltas(const std::vector<service::counterparty_line_delta>& deltas);

}

#endif
