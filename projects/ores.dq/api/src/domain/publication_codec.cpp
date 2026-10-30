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
#include "ores.dq.api/domain/publication_codec.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include <tuple>

namespace ores::dq::domain {

namespace {

/**
 * @brief The members of publication, named as the wire names them and
 * in the order the type declares them.
 */
constexpr auto publication_codec_fields = std::tuple{
    utility::codec::field{"tenant_id", &publication::tenant_id},
    utility::codec::field{"id", &publication::id},
    utility::codec::field{"dataset_id", &publication::dataset_id},
    utility::codec::field{"dataset_code", &publication::dataset_code},
    utility::codec::field{"mode", &publication::mode},
    utility::codec::field{"target_table", &publication::target_table},
    utility::codec::field{"records_inserted", &publication::records_inserted},
    utility::codec::field{"records_updated", &publication::records_updated},
    utility::codec::field{"records_skipped", &publication::records_skipped},
    utility::codec::field{"records_deleted", &publication::records_deleted},
    utility::codec::field{"published_by", &publication::published_by},
    utility::codec::field{"published_at", &publication::published_at},
};

}

std::string publication_codec::to_json(const publication& v) {
    return utility::codec::object::write_json(v, publication_codec_fields);
}

publication publication_codec::from_json(std::string_view text) {
    return utility::codec::object::read_json<publication>(text, publication_codec_fields);
}

std::string publication_codec::to_msgpack(const publication& v) {
    return utility::codec::object::write_msgpack(v, publication_codec_fields);
}

publication publication_codec::from_msgpack(std::span<const std::byte> data) {
    return utility::codec::object::read_msgpack<publication>(data, publication_codec_fields);
}

}
