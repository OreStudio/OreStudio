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
#ifndef ORES_TRADING_API_DOMAIN_TRADE_AUDIT_HPP
#define ORES_TRADING_API_DOMAIN_TRADE_AUDIT_HPP

#include <chrono>
#include <string>

namespace ores::trading::domain {

/**
 * @brief Audit trail fields for a trade.
 *
 * One of five field-groups that compose trading::domain::trade via
 * rfl::Flatten<T>.  See trade_identity_field_group.json for the
 * rationale behind the sub-struct decomposition.
 *
 * This group carries the provenance/audit fields that record who
 * changed the trade, why, and when.  These mirror the standard
 * ORE audit columns present on all temporal entities.
 */
struct trade_audit {
    /**
     * @brief Username of the system actor that wrote this record.
     */
    std::string modified_by;

    /**
     * @brief Username of the business user on whose behalf the change was made.
     */
    std::string performed_by;

    /**
     * @brief Structured reason code for the lifecycle event (e.g. NEW, AMEND, CANCEL).
     */
    std::string change_reason_code;

    /**
     * @brief Free-text commentary describing the reason for the change.
     */
    std::string change_commentary;

    /**
     * @brief Wall-clock timestamp at which this version of the trade was persisted.
     */
    std::chrono::system_clock::time_point recorded_at;
};

}

#endif
