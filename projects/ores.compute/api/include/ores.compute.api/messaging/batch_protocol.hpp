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
#ifndef ORES_COMPUTE_API_MESSAGING_BATCH_PROTOCOL_HPP
#define ORES_COMPUTE_API_MESSAGING_BATCH_PROTOCOL_HPP

#include "ores.compute.api/domain/batch.hpp"
#include <cstdint>
#include <string>
#include <vector>

namespace ores::compute::messaging {

struct get_batches_request {
    using response_type = struct get_batches_response;
    static constexpr std::string_view nats_subject = "compute.v1.batches.list";
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_batches_response {
    std::vector<ores::compute::domain::batch> batches;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

struct save_batch_request {
    using response_type = struct save_batch_response;
    static constexpr std::string_view nats_subject = "compute.v1.batches.save";
    ores::compute::domain::batch data;

    static save_batch_request from(ores::compute::domain::batch v) {
        return {.data = std::move(v)};
    }
};

struct save_batch_response {
    bool success = false;
    std::string message;
};

struct delete_batch_request {
    using response_type = struct delete_batch_response;
    static constexpr std::string_view nats_subject = "compute.v1.batches.delete";
    std::vector<std::string> ids;
};

struct delete_batch_response {
    bool success = false;
    std::string message;
};

struct get_batch_history_request {
    using response_type = struct get_batch_history_response;
    static constexpr std::string_view nats_subject = "compute.v1.batches.history";
    std::string id;
};

struct get_batch_history_response {
    std::vector<ores::compute::domain::batch> history;
    bool success = false;
    std::string message;
};

}

#endif
