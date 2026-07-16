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
#ifndef ORES_SYNTHETIC_API_MESSAGING_FOLDER_PROTOCOL_HPP
#define ORES_SYNTHETIC_API_MESSAGING_FOLDER_PROTOCOL_HPP

#include "ores.synthetic.api/domain/folder.hpp"
#include "ores.utility/domain/hierarchy.hpp"
#include <cstdint>
#include <string>
#include <vector>

namespace ores::synthetic::messaging {

struct get_folders_request {
    using response_type = struct get_folders_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.folders.list";
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_folders_response {
    std::vector<ores::synthetic::domain::folder> folders;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

struct save_folder_request {
    using response_type = struct save_folder_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.folders.save";
    ores::synthetic::domain::folder data;

    static save_folder_request from(ores::synthetic::domain::folder v) {
        return {.data = std::move(v)};
    }
};

struct save_folder_response {
    bool success = false;
    std::string message;
};

struct delete_folder_request {
    using response_type = struct delete_folder_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.folders.delete";
    std::vector<std::string> ids;
};

struct delete_folder_response {
    bool success = false;
    std::string message;
};

struct get_folder_history_request {
    using response_type = struct get_folder_history_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.folders.history";
    std::string id;
};

struct get_folder_history_response {
    std::vector<ores::synthetic::domain::folder> history;
    bool success = false;
    std::string message;
};

/**
 * @brief Reads the folder hierarchy rooted at, or containing,
 * a given folder.
 */
struct get_folder_hierarchy_request {
    using response_type = struct get_folder_hierarchy_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.folders.hierarchy";
    std::string root_id;
    bool from_root = false;
};

struct get_folder_hierarchy_response {
    bool success = false;
    std::string message;
    std::vector<ores::utility::domain::hierarchy_node> roots;
};

}

#endif
