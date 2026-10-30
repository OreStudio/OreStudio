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
#include "ores.dq.api/domain/lei_bic_codec.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include <tuple>

namespace ores::dq::domain {

namespace {

/**
 * @brief The members of lei_bic, named as the wire names them and
 * in the order the type declares them.
 */
constexpr auto lei_bic_codec_fields = std::tuple{
    utility::codec::field{"tenant_id", &lei_bic::tenant_id},
    utility::codec::field{"lei", &lei_bic::lei},
    utility::codec::field{"bic", &lei_bic::bic},
};

}

std::string lei_bic_codec::to_json(const lei_bic& v) {
    return utility::codec::object::write_json(v, lei_bic_codec_fields);
}

lei_bic lei_bic_codec::from_json(std::string_view text) {
    return utility::codec::object::read_json<lei_bic>(text, lei_bic_codec_fields);
}

std::string lei_bic_codec::to_msgpack(const lei_bic& v) {
    return utility::codec::object::write_msgpack(v, lei_bic_codec_fields);
}

lei_bic lei_bic_codec::from_msgpack(std::span<const std::byte> data) {
    return utility::codec::object::read_msgpack<lei_bic>(data, lei_bic_codec_fields);
}

}
