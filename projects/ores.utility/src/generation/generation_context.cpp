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
#include "ores.utility/generation/generation_context.hpp"

namespace ores::utility::generation {

generation_context::generation_context(std::uint64_t seed, entries initial)
    : engine_(std::make_shared<generation_engine>(seed)),
      env_(std::make_shared<generation_environment>(std::move(initial))) {}

generation_context::generation_context(entries initial)
    : engine_(std::make_shared<generation_engine>()),
      env_(std::make_shared<generation_environment>(std::move(initial))) {}

generation_context::generation_context(
    std::shared_ptr<generation_engine> engine,
    std::shared_ptr<const generation_environment> env)
    : engine_(std::move(engine)),
      env_(std::move(env)) {}

generation_context generation_context::child(entries overrides) const {
    auto child_env = std::make_shared<generation_environment>(
        env_, std::move(overrides));
    return generation_context(engine_, std::move(child_env));
}

}
