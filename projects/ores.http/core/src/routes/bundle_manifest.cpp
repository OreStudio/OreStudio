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
#include "ores.http.core/routes/bundle_manifest.hpp"
#include <stdexcept>
#include <system_error>

namespace ores::http::server::routes {

namespace {

/// The suffix a prebuilt brotli copy carries.
constexpr std::string_view brotli_suffix = ".br";

}

bundle_manifest::bundle_manifest(std::filesystem::path root)
    : root_(std::move(root)) {

    std::error_code ec;
    if (!std::filesystem::is_directory(root_, ec))
        throw std::runtime_error("The bundle root is not a directory: " + root_.string());

    for (const auto& item : std::filesystem::recursive_directory_iterator(root_, ec)) {
        std::error_code item_ec;
        if (!item.is_regular_file(item_ec))
            continue;

        auto path = std::filesystem::relative(item.path(), root_, item_ec).generic_string();
        if (item_ec)
            continue;

        /*
         * A dot file or a dot directory is the build's own bookkeeping, and a brotli
         * copy is not a file a browser asks for; both are recorded only as the
         * compression a neighbour offers.
         */
        if (path.starts_with(".") || path.find("/.") != std::string::npos)
            continue;
        if (path.ends_with(brotli_suffix))
            continue;

        bundle_entry entry;
        entry.path = path;
        entry.size = item.file_size(item_ec);
        entry.compressed = std::filesystem::exists(root_ / (path + std::string(brotli_suffix)));
        entries_.insert_or_assign(std::move(path), std::move(entry));
    }
}

const bundle_entry* bundle_manifest::find(const std::string& path) const {
    const auto it = entries_.find(path);
    return it == entries_.end() ? nullptr : &it->second;
}

}
