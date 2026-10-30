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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_domain_type_codec.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.dq.api/domain/fsm_transition_codec.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include <tuple>

namespace ores::dq::domain {

namespace {

/**
 * @brief The members of fsm_transition, named as the wire names them and
 * in the order the type declares them.
 */
constexpr auto fsm_transition_codec_fields = std::tuple{
    utility::codec::field{"version", &fsm_transition::version},
    utility::codec::field{"tenant_id", &fsm_transition::tenant_id},
    utility::codec::field{"id", &fsm_transition::id},
    utility::codec::field{"machine_id", &fsm_transition::machine_id},
    utility::codec::field{"from_state_id", &fsm_transition::from_state_id},
    utility::codec::field{"to_state_id", &fsm_transition::to_state_id},
    utility::codec::field{"name", &fsm_transition::name},
    utility::codec::field{"guard_function", &fsm_transition::guard_function},
    utility::codec::field{"modified_by", &fsm_transition::modified_by},
    utility::codec::field{"performed_by", &fsm_transition::performed_by},
    utility::codec::field{"change_reason_code", &fsm_transition::change_reason_code},
    utility::codec::field{"change_commentary", &fsm_transition::change_commentary},
    utility::codec::field{"recorded_at", &fsm_transition::recorded_at},
};

}

std::string fsm_transition_codec::to_json(const fsm_transition& v) {
    return utility::codec::object::write_json(v, fsm_transition_codec_fields);
}

fsm_transition fsm_transition_codec::from_json(std::string_view text) {
    return utility::codec::object::read_json<fsm_transition>(text, fsm_transition_codec_fields);
}

std::string fsm_transition_codec::to_msgpack(const fsm_transition& v) {
    return utility::codec::object::write_msgpack(v, fsm_transition_codec_fields);
}

fsm_transition fsm_transition_codec::from_msgpack(std::span<const std::byte> data) {
    return utility::codec::object::read_msgpack<fsm_transition>(data, fsm_transition_codec_fields);
}

}
