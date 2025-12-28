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
#ifndef ORES_PLATFORM_TIME_TIME_POINT_PARSER_HPP
#define ORES_PLATFORM_TIME_TIME_POINT_PARSER_HPP

#include <chrono>
#include <string>
#include <rfl.hpp>
#include "ores.platform/time/datetime.hpp"

namespace rfl::parsing {

template <class ReaderType, class WriterType, class ProcessorsType>
struct Parser<ReaderType, WriterType,
              std::chrono::system_clock::time_point, ProcessorsType> {
    using InputVarType = typename ReaderType::InputVarType;
    using OutputVarType = typename WriterType::OutputVarType;

    static Result<std::chrono::system_clock::time_point> read(
        const ReaderType& _r, const InputVarType& _var) noexcept {
        const auto str_result = Parser<ReaderType, WriterType,
            std::string, ProcessorsType>::read(_r, _var);
        if (!str_result) {
            return rfl::Unexpected(Error(str_result.error()->what()));
        }
        try {
            return ores::platform::time::datetime::parse_time_point(
                str_result.value());
        } catch (const std::exception& e) {
            return rfl::Unexpected(Error(e.what()));
        }
    }

    template <class P>
    static void write(const WriterType& _w,
                      const std::chrono::system_clock::time_point& _tp,
                      const P& _parent) noexcept {
        const auto str = ores::platform::time::datetime::format_time_point(_tp);
        Parser<ReaderType, WriterType, std::string, ProcessorsType>::write(
            _w, str, _parent);
    }
};

}

#endif
