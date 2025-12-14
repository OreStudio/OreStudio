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
#include "ores.comms/messaging/compression.hpp"

#include <sstream>
#include <boost/iostreams/copy.hpp>
#include <boost/iostreams/device/array.hpp>
#include <boost/iostreams/device/back_inserter.hpp>
#include <boost/iostreams/filter/zlib.hpp>
#include <boost/iostreams/filter/gzip.hpp>
#include <boost/iostreams/filter/bzip2.hpp>
#include <boost/iostreams/filtering_streambuf.hpp>
#include "ores.utility/log/make_logger.hpp"

namespace {

std::string_view logger_name = "ores.comms.messaging.compression";

auto& lg() {
    using namespace ores::utility::log;
    static auto instance = make_logger(logger_name);
    return instance;
}

}

namespace ores::comms::messaging {

using namespace ores::utility::log;
namespace io = boost::iostreams;

bool is_compression_supported(compression_type type) {
    switch (type) {
        case compression_type::none:
        case compression_type::zlib:
        case compression_type::gzip:
        case compression_type::bzip2:
            return true;
        default:
            return false;
    }
}

std::expected<std::vector<std::byte>, error_code>
compress(std::span<const std::byte> data, compression_type type) {
    if (type == compression_type::none) {
        return std::vector<std::byte>(data.begin(), data.end());
    }

    if (!is_compression_supported(type)) {
        BOOST_LOG_SEV(lg(), error) << "Unsupported compression type: " << type;
        return std::unexpected(error_code::unsupported_compression);
    }

    try {
        std::vector<char> compressed;
        io::filtering_streambuf<io::output> out;

        switch (type) {
            case compression_type::zlib:
                out.push(io::zlib_compressor());
                break;
            case compression_type::gzip:
                out.push(io::gzip_compressor());
                break;
            case compression_type::bzip2:
                out.push(io::bzip2_compressor());
                break;
            default:
                return std::unexpected(error_code::unsupported_compression);
        }

        out.push(io::back_inserter(compressed));

        io::array_source source(reinterpret_cast<const char*>(data.data()), data.size());
        io::copy(source, out);

        std::vector<std::byte> result(compressed.size());
        std::memcpy(result.data(), compressed.data(), compressed.size());

        BOOST_LOG_SEV(lg(), debug) << "Compressed " << data.size() << " bytes to "
                                   << result.size() << " bytes using " << type;
        return result;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Compression failed: " << e.what();
        return std::unexpected(error_code::decompression_failed);
    }
}

std::expected<std::vector<std::byte>, error_code>
decompress(std::span<const std::byte> data, compression_type type) {
    if (type == compression_type::none) {
        return std::vector<std::byte>(data.begin(), data.end());
    }

    if (!is_compression_supported(type)) {
        BOOST_LOG_SEV(lg(), error) << "Unsupported compression type: " << type;
        return std::unexpected(error_code::unsupported_compression);
    }

    try {
        std::vector<char> decompressed;
        io::filtering_streambuf<io::input> in;

        switch (type) {
            case compression_type::zlib:
                in.push(io::zlib_decompressor());
                break;
            case compression_type::gzip:
                in.push(io::gzip_decompressor());
                break;
            case compression_type::bzip2:
                in.push(io::bzip2_decompressor());
                break;
            default:
                return std::unexpected(error_code::unsupported_compression);
        }

        io::array_source source(reinterpret_cast<const char*>(data.data()), data.size());
        in.push(source);

        io::back_insert_device<std::vector<char>> sink(decompressed);
        io::copy(in, sink);

        std::vector<std::byte> result(decompressed.size());
        std::memcpy(result.data(), decompressed.data(), decompressed.size());

        BOOST_LOG_SEV(lg(), debug) << "Decompressed " << data.size() << " bytes to "
                                   << result.size() << " bytes using " << type;
        return result;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Decompression failed: " << e.what();
        return std::unexpected(error_code::decompression_failed);
    }
}

}
