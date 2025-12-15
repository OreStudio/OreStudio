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
#include "ores.shell/app/commands/compression_commands.hpp"

#include <ostream>
#include <functional>
#include <cli/cli.h>

namespace ores::shell::app::commands {

using namespace ores::utility::log;
using comms::messaging::COMPRESSION_SUPPORT_ZLIB;
using comms::messaging::COMPRESSION_SUPPORT_GZIP;
using comms::messaging::COMPRESSION_SUPPORT_BZIP2;
using comms::messaging::COMPRESSION_SUPPORT_ALL;

// Initialize with all compression algorithms enabled by default
std::uint8_t compression_commands::supported_compression_ = COMPRESSION_SUPPORT_ALL;

namespace {

std::string compression_status_string(std::uint8_t supported) {
    if (supported == 0) {
        return "off";
    }

    std::string result = "on (";
    bool first = true;

    if (supported & COMPRESSION_SUPPORT_ZLIB) {
        result += "zlib";
        first = false;
    }
    if (supported & COMPRESSION_SUPPORT_GZIP) {
        if (!first) result += ", ";
        result += "gzip";
        first = false;
    }
    if (supported & COMPRESSION_SUPPORT_BZIP2) {
        if (!first) result += ", ";
        result += "bzip2";
    }

    result += ")";
    return result;
}

} // anonymous namespace

void compression_commands::register_commands(cli::Menu& root_menu) {
    root_menu.Insert("compression",
        [](std::ostream& out, std::string enable, std::string algorithm) {
            process_compression(std::ref(out), std::move(enable),
                std::move(algorithm));
        },
        "Set compression: 'compression off', 'compression on', "
        "'compression on <algo>' (zlib/gzip/bzip2)");

    // Also register a status-only variant
    root_menu.Insert("compression",
        [](std::ostream& out) {
            out << "Compression: " << compression_status_string(supported_compression_)
                << std::endl;
        },
        "Show current compression setting");
}

void compression_commands::process_compression(std::ostream& out,
    const std::string& enable, const std::string& algorithm) {

    if (enable == "off") {
        supported_compression_ = 0;
        BOOST_LOG_SEV(lg(), info) << "Compression disabled";
        out << "Compression disabled. New connections will not use compression."
            << std::endl;
        return;
    }

    if (enable == "on") {
        if (algorithm.empty()) {
            // Enable all compression algorithms
            supported_compression_ = COMPRESSION_SUPPORT_ALL;
            BOOST_LOG_SEV(lg(), info) << "Compression enabled with all algorithms";
            out << "Compression enabled with all algorithms (zlib, gzip, bzip2)."
                << std::endl;
            out << "Server will select the best algorithm during handshake."
                << std::endl;
        } else if (algorithm == "zlib") {
            supported_compression_ = COMPRESSION_SUPPORT_ZLIB;
            BOOST_LOG_SEV(lg(), info) << "Compression enabled with zlib only";
            out << "Compression enabled with zlib only." << std::endl;
        } else if (algorithm == "gzip") {
            supported_compression_ = COMPRESSION_SUPPORT_GZIP;
            BOOST_LOG_SEV(lg(), info) << "Compression enabled with gzip only";
            out << "Compression enabled with gzip only." << std::endl;
        } else if (algorithm == "bzip2") {
            supported_compression_ = COMPRESSION_SUPPORT_BZIP2;
            BOOST_LOG_SEV(lg(), info) << "Compression enabled with bzip2 only";
            out << "Compression enabled with bzip2 only." << std::endl;
        } else {
            out << "Unknown compression algorithm: " << algorithm << std::endl;
            out << "Valid algorithms: zlib, gzip, bzip2" << std::endl;
            return;
        }
        out << "Setting takes effect on next connection." << std::endl;
        return;
    }

    // Invalid command
    out << "Usage: compression off | compression on [algorithm]" << std::endl;
    out << "  compression off         - Disable compression" << std::endl;
    out << "  compression on          - Enable all compression algorithms" << std::endl;
    out << "  compression on zlib     - Enable zlib compression only" << std::endl;
    out << "  compression on gzip     - Enable gzip compression only" << std::endl;
    out << "  compression on bzip2    - Enable bzip2 compression only" << std::endl;
}

}
