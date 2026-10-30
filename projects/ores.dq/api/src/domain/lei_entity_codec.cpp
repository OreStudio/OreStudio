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
#include "ores.dq.api/domain/lei_entity_codec.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include <tuple>

namespace ores::dq::domain {

namespace {

/**
 * @brief The members of lei_entity, named as the wire names them and
 * in the order the type declares them.
 */
constexpr auto lei_entity_codec_fields = std::tuple{
    utility::codec::field{"tenant_id", &lei_entity::tenant_id},
    utility::codec::field{"lei", &lei_entity::lei},
    utility::codec::field{"entity_legal_name", &lei_entity::entity_legal_name},
    utility::codec::field{"entity_entity_category", &lei_entity::entity_entity_category},
    utility::codec::field{"entity_entity_sub_category", &lei_entity::entity_entity_sub_category},
    utility::codec::field{"entity_entity_status", &lei_entity::entity_entity_status},
    utility::codec::field{"entity_legal_form_entity_legal_form_code",
                          &lei_entity::entity_legal_form_entity_legal_form_code},
    utility::codec::field{"entity_legal_form_other_legal_form",
                          &lei_entity::entity_legal_form_other_legal_form},
    utility::codec::field{"entity_legal_jurisdiction", &lei_entity::entity_legal_jurisdiction},
    utility::codec::field{"entity_legal_address_first_address_line",
                          &lei_entity::entity_legal_address_first_address_line},
    utility::codec::field{"entity_legal_address_city", &lei_entity::entity_legal_address_city},
    utility::codec::field{"entity_legal_address_region", &lei_entity::entity_legal_address_region},
    utility::codec::field{"entity_legal_address_country",
                          &lei_entity::entity_legal_address_country},
    utility::codec::field{"entity_legal_address_postal_code",
                          &lei_entity::entity_legal_address_postal_code},
    utility::codec::field{"entity_headquarters_address_first_address_line",
                          &lei_entity::entity_headquarters_address_first_address_line},
    utility::codec::field{"entity_headquarters_address_city",
                          &lei_entity::entity_headquarters_address_city},
    utility::codec::field{"entity_headquarters_address_region",
                          &lei_entity::entity_headquarters_address_region},
    utility::codec::field{"entity_headquarters_address_country",
                          &lei_entity::entity_headquarters_address_country},
    utility::codec::field{"entity_headquarters_address_postal_code",
                          &lei_entity::entity_headquarters_address_postal_code},
    utility::codec::field{"entity_entity_creation_date", &lei_entity::entity_entity_creation_date},
    utility::codec::field{"registration_initial_registration_date",
                          &lei_entity::registration_initial_registration_date},
    utility::codec::field{"registration_last_update_date",
                          &lei_entity::registration_last_update_date},
    utility::codec::field{"registration_next_renewal_date",
                          &lei_entity::registration_next_renewal_date},
    utility::codec::field{"registration_registration_status",
                          &lei_entity::registration_registration_status},
    utility::codec::field{"entity_transliterated_name_1",
                          &lei_entity::entity_transliterated_name_1},
    utility::codec::field{"entity_transliterated_name_1_type",
                          &lei_entity::entity_transliterated_name_1_type},
};

}

std::string lei_entity_codec::to_json(const lei_entity& v) {
    return utility::codec::object::write_json(v, lei_entity_codec_fields);
}

lei_entity lei_entity_codec::from_json(std::string_view text) {
    return utility::codec::object::read_json<lei_entity>(text, lei_entity_codec_fields);
}

std::string lei_entity_codec::to_msgpack(const lei_entity& v) {
    return utility::codec::object::write_msgpack(v, lei_entity_codec_fields);
}

lei_entity lei_entity_codec::from_msgpack(std::span<const std::byte> data) {
    return utility::codec::object::read_msgpack<lei_entity>(data, lei_entity_codec_fields);
}

}
