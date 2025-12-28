/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.telemetry/export/upload_position_tracker.hpp"

#include <fstream>
#include "ores.telemetry/log/make_logger.hpp"

namespace ores::telemetry::exp {

namespace {

auto& lg() {
    using namespace ores::telemetry::log;
    static auto instance = make_logger("ores.telemetry.export.upload_position_tracker");
    return instance;
}

}

upload_position_tracker::upload_position_tracker(
    std::filesystem::path log_file_path)
    : log_file_path_(std::move(log_file_path)),
      marker_file_path_(log_file_path_.string() + ".uploaded"),
      position_(load_position_from_file()) {
}

std::uint64_t upload_position_tracker::get_position() const {
    std::lock_guard<std::mutex> lock(mutex_);
    return position_;
}

void upload_position_tracker::set_position(std::uint64_t position) {
    std::lock_guard<std::mutex> lock(mutex_);
    position_ = position;
    save_position_to_file(position);
}

const std::filesystem::path& upload_position_tracker::marker_file_path() const {
    return marker_file_path_;
}

void upload_position_tracker::reset() {
    std::lock_guard<std::mutex> lock(mutex_);
    position_ = 0;

    std::error_code ec;
    std::filesystem::remove(marker_file_path_, ec);
    // Ignore errors - file might not exist
}

std::uint64_t upload_position_tracker::load_position_from_file() const {
    std::ifstream file(marker_file_path_, std::ios::binary);
    if (!file) {
        return 0;
    }

    std::uint64_t position = 0;
    file.read(reinterpret_cast<char*>(&position), sizeof(position));

    if (!file) {
        return 0;
    }

    return position;
}

void upload_position_tracker::save_position_to_file(
    std::uint64_t position) const {
    using namespace ores::telemetry::log;

    std::ofstream file(marker_file_path_,
        std::ios::binary | std::ios::trunc);

    if (!file) {
        BOOST_LOG_SEV(lg(), warn)
            << "Failed to save upload position to marker file: "
            << marker_file_path_
            << ". Telemetry records may be re-uploaded on restart.";
        return;
    }

    file.write(reinterpret_cast<const char*>(&position), sizeof(position));
    file.flush();
}

}
