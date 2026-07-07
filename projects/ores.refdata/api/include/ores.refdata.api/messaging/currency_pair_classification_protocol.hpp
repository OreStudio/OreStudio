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
#ifndef ORES_REFDATA_API_MESSAGING_CURRENCY_PAIR_CLASSIFICATION_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_CURRENCY_PAIR_CLASSIFICATION_PROTOCOL_HPP

#include "ores.refdata.api/domain/currency_pair_classification.hpp"
#include <cstdint>
#include <string>
#include <vector>

namespace ores::refdata::messaging {

struct get_currency_pair_classifications_request {
    using response_type = struct get_currency_pair_classifications_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.currency_pair_classifications.list";
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_currency_pair_classifications_response {
    std::vector<ores::refdata::domain::currency_pair_classification> classifications;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

struct save_currency_pair_classification_request {
    using response_type = struct save_currency_pair_classification_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.currency_pair_classifications.save";
    ores::refdata::domain::currency_pair_classification data;

    static save_currency_pair_classification_request
    from(ores::refdata::domain::currency_pair_classification v) {
        return {.data = std::move(v)};
    }
};

struct save_currency_pair_classification_response {
    bool success = false;
    std::string message;
};

struct delete_currency_pair_classification_request {
    using response_type = struct delete_currency_pair_classification_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.currency_pair_classifications.delete";
    std::vector<std::string> codes;
};

struct delete_currency_pair_classification_response {
    bool success = false;
    std::string message;
};

struct get_currency_pair_classification_history_request {
    using response_type = struct get_currency_pair_classification_history_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.currency_pair_classifications.history";
    std::string code;
};

struct get_currency_pair_classification_history_response {
    std::vector<ores::refdata::domain::currency_pair_classification> history;
    bool success = false;
    std::string message;
};

}

#endif
