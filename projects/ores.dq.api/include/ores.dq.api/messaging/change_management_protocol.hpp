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
#ifndef ORES_DQ_API_MESSAGING_CHANGE_MANAGEMENT_PROTOCOL_HPP
#define ORES_DQ_API_MESSAGING_CHANGE_MANAGEMENT_PROTOCOL_HPP

#include <string>
#include <string_view>
#include <vector>
#include "ores.dq.api/domain/change_reason_category.hpp"
#include "ores.dq.api/domain/change_reason.hpp"

namespace ores::dq::messaging {

// =============================================================================
// Change Reason Category Protocol
// =============================================================================

struct get_change_reason_categories_request {
    using response_type = struct get_change_reason_categories_response;
    static constexpr std::string_view nats_subject =
        "dq.v1.change-reason-categories.list";
};

struct get_change_reason_categories_response {
    std::vector<ores::dq::domain::change_reason_category> categories;
};

struct save_change_reason_category_request {
    using response_type = struct save_change_reason_category_response;
    static constexpr std::string_view nats_subject =
        "dq.v1.change-reason-categories.save";
    ores::dq::domain::change_reason_category data;

    static save_change_reason_category_request
    from(ores::dq::domain::change_reason_category c) {
        return { .data = std::move(c) };
    }
};

struct save_change_reason_category_response {
    bool success = false;
    std::string message;
};

struct delete_change_reason_category_request {
    using response_type = struct delete_change_reason_category_response;
    static constexpr std::string_view nats_subject =
        "dq.v1.change-reason-categories.delete";
    std::vector<std::string> codes;
};

struct delete_change_reason_category_response {
    bool success = false;
    std::string message;
};

struct get_change_reason_category_history_request {
    using response_type = struct get_change_reason_category_history_response;
    static constexpr std::string_view nats_subject =
        "dq.v1.change-reason-categories.history";
    std::string code;
};

struct get_change_reason_category_history_response {
    bool success = false;
    std::string message;
    std::vector<ores::dq::domain::change_reason_category> versions;
};

// =============================================================================
// Change Reason Protocol
// =============================================================================

struct get_change_reasons_request {
    using response_type = struct get_change_reasons_response;
    static constexpr std::string_view nats_subject =
        "dq.v1.change-reasons.list";
};

struct get_change_reasons_response {
    std::vector<ores::dq::domain::change_reason> reasons;
};

struct save_change_reason_request {
    using response_type = struct save_change_reason_response;
    static constexpr std::string_view nats_subject =
        "dq.v1.change-reasons.save";
    ores::dq::domain::change_reason data;

    static save_change_reason_request from(ores::dq::domain::change_reason r) {
        return { .data = std::move(r) };
    }
};

struct save_change_reason_response {
    bool success = false;
    std::string message;
};

struct delete_change_reason_request {
    using response_type = struct delete_change_reason_response;
    static constexpr std::string_view nats_subject =
        "dq.v1.change-reasons.delete";
    std::vector<std::string> codes;
};

struct delete_change_reason_response {
    bool success = false;
    std::string message;
};

struct get_change_reason_history_request {
    using response_type = struct get_change_reason_history_response;
    static constexpr std::string_view nats_subject =
        "dq.v1.change-reasons.history";
    std::string code;
};

struct get_change_reason_history_response {
    bool success = false;
    std::string message;
    std::vector<ores::dq::domain::change_reason> versions;
};

}

#endif
