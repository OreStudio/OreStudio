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
#ifndef ORES_HTTP_ROUTES_REPORTING_CUT_KIND_ROWS_HPP
#define ORES_HTTP_ROUTES_REPORTING_CUT_KIND_ROWS_HPP

#include "ores.http.api/domain/http_request.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.reporting.api/domain/cut_kind.hpp"
#include "ores.reporting.api/messaging/report_operations_protocol.hpp"
#include <map>
#include <optional>
#include <string>
#include <vector>

namespace ores::http::routes::reporting::cuts {

/// One approval kind the inbox holds, as a kind of cut points at it.
struct approval_kind final {
    std::string code;
    std::string name;
    std::optional<int> approvals_required;
    std::optional<std::string> decide_permission_code;
};

/**
 * @brief The words a kind of cut's three codes stand for.
 *
 * The family and the resolution rule are the reporting component's own and the
 * approval kind is the inbox's, so this reads three closed sets and joins them
 * beside the kind. A refusal of the inbox read is an empty set, because a kind
 * states whether a signature is required without naming who decides it; any
 * other failure is raised.
 */
struct kind_lookups final {
    std::map<std::string, std::string> families;
    std::map<std::string, std::string> rules;
    std::map<std::string, approval_kind> approvals;
};

kind_lookups read_kind_lookups(const ores::http::domain::http_request& req,
                               ores::nats::service::nats_client& session);

/// One kind of cut as the type list and the type record read it.
struct kind_row final {
    std::string code;
    std::string name;
    std::string description;
    std::string family_code;
    std::string family_name;
    bool signed_by_kind = false;
    std::optional<std::string> approval_kind_code;
    std::optional<std::string> approval_kind_name;
    std::optional<int> approvals_required;
    std::optional<std::string> decide_permission_code;
    bool output_is_indicative = false;
    std::string resolution_rule_code;
    std::string resolution_rule_name;
    int retention_months = 0;
    std::string retention_destination_code;
    bool legal_hold_default = false;
    int display_order = 0;
    int cuts_count = 0;
    int version = 0;
};

kind_row row_of(const ores::reporting::domain::cut_kind& kind,
                const kind_lookups& lookups,
                int cuts_count);

/// One kind, with the count of the party's cuts of it, or nothing when the
/// store holds none of that code.
std::optional<kind_row> read_one_kind(const ores::http::domain::http_request& req,
                                      ores::nats::service::nats_client& session,
                                      const std::string& code);

/**
 * @brief The kind of a cut whose kind the store no longer holds.
 *
 * A retired kind leaves its cuts behind, and a cut records what it copied from
 * its kind when it was taken. The record states those facts rather than
 * refusing to show a body that exists.
 */
kind_row row_of_a_cut(const ores::reporting::domain::cut& cut);

/// One kind as the screen reads it.
std::string kind_json(const kind_row& row);

/// One cut of a list as the screen reads it.
std::string cut_row_json(const ores::reporting::messaging::cut_row& row);

}

#endif
