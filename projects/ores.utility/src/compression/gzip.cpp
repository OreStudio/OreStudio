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
#include "ores.utility/compression/gzip.hpp"

#include <sstream>
#include <stdexcept>
#include <boost/iostreams/filtering_stream.hpp>
#include <boost/iostreams/filter/gzip.hpp>
#include <boost/iostreams/copy.hpp>
#include <boost/iostreams/device/array.hpp>
#include <boost/iostreams/device/back_inserter.hpp>

namespace ores::utility::compression {

namespace io = boost::iostreams;

std::vector<char> gzip_compress(std::span<const char> input) {
    std::vector<char> output;
    io::filtering_ostream out;
    out.push(io::gzip_compressor());
    out.push(io::back_inserter(output));
    io::write(out, input.data(), static_cast<std::streamsize>(input.size()));
    io::close(out);
    return output;
}

std::vector<char> gzip_decompress(std::span<const char> input) {
    io::array_source src(input.data(), input.size());
    io::filtering_istream in;
    in.push(io::gzip_decompressor());
    in.push(src);

    std::vector<char> output;
    io::back_insert_device<std::vector<char>> sink(output);
    io::copy(in, sink);
    return output;
}

}
