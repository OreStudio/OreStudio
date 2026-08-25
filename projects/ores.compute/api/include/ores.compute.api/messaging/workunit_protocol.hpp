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
#ifndef ORES_COMPUTE_API_MESSAGING_WORKUNIT_PROTOCOL_HPP
#define ORES_COMPUTE_API_MESSAGING_WORKUNIT_PROTOCOL_HPP

#include "ores.compute.api/domain/workunit.hpp"
#include <cstdint>
#include <string>
#include <vector>

namespace ores::compute::messaging {

struct get_workunits_request {
    using response_type = struct get_workunits_response;
    static constexpr std::string_view nats_subject = "compute.v1.workunits.list";
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_workunits_response {
    std::vector<ores::compute::domain::workunit> workunits;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

struct save_workunit_request {
    using response_type = struct save_workunit_response;
    static constexpr std::string_view nats_subject = "compute.v1.workunits.save";
    ores::compute::domain::workunit data;

    static save_workunit_request from(ores::compute::domain::workunit v) {
        return {.data = std::move(v)};
    }
};

struct save_workunit_response {
    bool success = false;
    std::string message;
};

struct delete_workunit_request {
    using response_type = struct delete_workunit_response;
    static constexpr std::string_view nats_subject = "compute.v1.workunits.delete";
    std::vector<std::string> ids;
};

struct delete_workunit_response {
    bool success = false;
    std::string message;
};

struct get_workunit_history_request {
    using response_type = struct get_workunit_history_response;
    static constexpr std::string_view nats_subject = "compute.v1.workunits.history";
    std::string id;
};

struct get_workunit_history_response {
    std::vector<ores::compute::domain::workunit> history;
    bool success = false;
    std::string message;
};

struct get_workunits_by_batch_id_request {
    using response_type = struct get_workunits_by_batch_id_response;
    static constexpr std::string_view nats_subject = "compute.v1.workunits.list_by_batch_id";
    std::string batch_id;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_workunits_by_batch_id_response {
    std::vector<ores::compute::domain::workunit> workunits;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

}

#endif
