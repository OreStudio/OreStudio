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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_ORE_CORE_XML_STABLE_ARENA_HPP
#define ORES_ORE_CORE_XML_STABLE_ARENA_HPP

#include <cstddef>
#include <memory>
#include <vector>

namespace ores::ore::xml {

/**
 * @brief A growing sequence that never moves an element and may be declared
 * while its element type is incomplete.
 *
 * A recursive ORE type holds an arena of a record that is defined after it.
 * std::deque needs the element type complete where the deque is declared on
 * MSVC, so a member of that kind does not compile there. A vector of owning
 * pointers needs no complete type until a member function is used, and keeps
 * every element at one address for the life of the arena.
 */
template <typename T>
class stable_arena final {
public:
    stable_arena() = default;
    stable_arena(stable_arena&&) noexcept = default;
    stable_arena& operator=(stable_arena&&) noexcept = default;

    stable_arena(const stable_arena& other) {
        items_.reserve(other.items_.size());
        for (const auto& item : other.items_)
            items_.push_back(std::make_unique<T>(*item));
    }

    stable_arena& operator=(const stable_arena& other) {
        if (this != &other) {
            stable_arena copy(other);
            items_ = std::move(copy.items_);
        }
        return *this;
    }

    T& emplace_back() {
        items_.push_back(std::make_unique<T>());
        return *items_.back();
    }

    T& back() {
        return *items_.back();
    }
    const T& back() const {
        return *items_.back();
    }
    std::size_t size() const {
        return items_.size();
    }
    T& operator[](std::size_t index) {
        return *items_[index];
    }
    const T& operator[](std::size_t index) const {
        return *items_[index];
    }

private:
    std::vector<std::unique_ptr<T>> items_;
};

}

#endif
