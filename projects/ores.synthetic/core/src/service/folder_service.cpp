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
#include "ores.synthetic.core/service/folder_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::synthetic::service {

using namespace ores::logging;

folder_service::folder_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::folder> folder_service::list_folders(std::uint32_t offset,
                                                         std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all folders";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t folder_service::count_folders() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total folders count";
    return repo_.get_total_folder_count(ctx_);
}

std::optional<domain::folder> folder_service::get_folder_at_version(const std::string& id,
                                                                    std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting folder at version: " << id << " version: " << version;
    return repo_.read_at_version(ctx_, id, version);
}

std::optional<domain::folder> folder_service::get_folder(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting folder: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void folder_service::save_folder(const domain::folder& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Folder id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving folder: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved folder: " << v.id;
}

void folder_service::save_folders(const std::vector<domain::folder>& folders) {
    for (const auto& e : folders)
        if (e.id.is_nil())
            throw std::invalid_argument("Folder id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving " << folders.size() << " folders";
    auto ts = folders;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void folder_service::delete_folder(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing folder: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed folder: " << id;
}

void folder_service::delete_folders(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::folder> folder_service::get_folder_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for folder: " << id;
    return repo_.read_all(ctx_, id);
}

std::vector<ores::utility::domain::hierarchy_node>
folder_service::get_hierarchy(const boost::uuids::uuid& root_id, bool from_root) {
    BOOST_LOG_SEV(lg(), debug) << "Getting hierarchy for folder root: " << root_id;
    auto rows = repo_.get_hierarchy(ctx_, root_id, from_root);
    return ores::utility::domain::build_tree(rows);
}

}
