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
#ifndef ORES_UTILITY_CONVERTER_BASE64_CONVERTER_HPP
#define ORES_UTILITY_CONVERTER_BASE64_CONVERTER_HPP

#include <string>
#include <vector>
#include <cstdint>
#include "ores.telemetry/log/make_logger.hpp"

namespace ores::utility::converter {

/**
 * @brief Minimal Base64 encoder.
 */
class base64_converter {
private:
    inline static std::string_view logger_name =
        "ores.utility.converter.base64_converter";

    [[nodiscard]] static auto& lg() {
        using namespace ores::telemetry::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Convert to Base64.
     */
    static std::string convert(const std::vector<uint8_t>& data);

    /**
     * @brief Convert from Base64.
     */
    static std::vector<uint8_t> convert(const std::string& data);
};

}

#endif
