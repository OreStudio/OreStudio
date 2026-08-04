# -*- mode: cmake; cmake-tab-width: 4; indent-tabs-mode: nil -*-
#
# Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51
# Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
set(files
    "compression/gzip.cpp"
    "convert/base32_converter.cpp"
    "convert/base64_converter.cpp"
    "crypto/sha256.cpp"
    "domain/hierarchy.cpp"
    "faker/datetime.cpp"
    "faker/internet.cpp"
    "faker/totp.cpp"
    "generation/generation_context.cpp"
    "generation/generation_engine.cpp"
    "generation/generation_environment.cpp"
    "generation/tree_builder.cpp"
    "program_options/common_configuration.cpp"
    "program_options/environment_mapper_factory.cpp"
    "program_options/shared_domain_registry.cpp"
    "serialization/reader.cpp"
    "serialization/writer.cpp"
    "string/converter.cpp"
    "string/short_code_generator.cpp"
    "uuid/tenant_id.cpp"
    "uuid/uuid_v7_generator.cpp"
)

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.utility/compression/gzip.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.utility/concurrency/retry_strategy.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.utility/convert/base32_converter.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.utility/convert/base64_converter.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.utility/convert/ores.utility.convert.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.utility/crypto/sha256.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.utility/domain/hierarchy.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.utility/export.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.utility/faker/datetime.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.utility/faker/internet.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.utility/faker/ores.utility.faker.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.utility/faker/totp.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.utility/generation/generation_context.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.utility/generation/generation_engine.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.utility/generation/generation_environment.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.utility/generation/generation_keys.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.utility/generation/tree_builder.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.utility/ores.utility.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.utility/program_options/common_configuration.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.utility/program_options/environment_mapper_factory.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.utility/program_options/ores.utility.program_options.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.utility/program_options/shared_domain_registry.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.utility/rfl/ores.utility.rfl.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.utility/rfl/reflectors.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.utility/rfl/time_point_parser.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.utility/serialization/error_code.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.utility/serialization/reader.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.utility/serialization/writer.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.utility/streaming/ores.utility.streaming.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.utility/streaming/std_optional.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.utility/streaming/std_vector.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.utility/streaming/streaming.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.utility/streaming/tenant_id_parser.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.utility/streaming/uuid_parser.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.utility/string/conversion_error.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.utility/string/converter.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.utility/string/ores.utility.string.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.utility/string/short_code_generator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.utility/string/string.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.utility/uuid/ores.utility.uuid.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.utility/uuid/tenant_id.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.utility/uuid/uuid_v7_generator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.utility/version/ores.utility.version.hpp"
)
