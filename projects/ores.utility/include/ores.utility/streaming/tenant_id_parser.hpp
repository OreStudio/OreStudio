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
#ifndef ORES_UTILITY_STREAMING_TENANT_ID_PARSER_HPP
#define ORES_UTILITY_STREAMING_TENANT_ID_PARSER_HPP

#include <string>
#include <rfl.hpp>
#include "ores.utility/uuid/tenant_id.hpp"

namespace rfl::parsing {

/**
 * @brief Custom reflect-cpp parser for tenant_id.
 *
 * Serializes tenant_id as a string (UUID format). This is necessary because
 * tenant_id has private members that prevent structured bindings.
 */
template <class ReaderType, class WriterType, class ProcessorsType>
struct Parser<ReaderType, WriterType, ores::utility::uuid::tenant_id, ProcessorsType> {
    using InputVarType = typename ReaderType::InputVarType;
    using OutputVarType = typename WriterType::OutputVarType;

    static Result<ores::utility::uuid::tenant_id> read(
        const ReaderType& _r, const InputVarType& _var) noexcept {
        const auto str_result = Parser<ReaderType, WriterType,
            std::string, ProcessorsType>::read(_r, _var);
        if (!str_result) {
            return rfl::Unexpected(Error(str_result.error()->what()));
        }
        auto tenant_result = ores::utility::uuid::tenant_id::from_string(
            str_result.value());
        if (!tenant_result) {
            return rfl::Unexpected(Error(tenant_result.error()));
        }
        return *tenant_result;
    }

    template <class P>
    static void write(const WriterType& _w,
                      const ores::utility::uuid::tenant_id& _tenant_id,
                      const P& _parent) noexcept {
        const auto str = _tenant_id.to_string();
        Parser<ReaderType, WriterType, std::string, ProcessorsType>::write(
            _w, str, _parent);
    }
};

}

#endif
