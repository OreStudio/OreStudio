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
#ifndef ORES_PLATFORM_FILESYSTEM_SCOPED_TEMP_DIRECTORY_HPP
#define ORES_PLATFORM_FILESYSTEM_SCOPED_TEMP_DIRECTORY_HPP

#include "ores.platform/export.hpp"
#include "ores.platform/filesystem/io_error.hpp"
#include <filesystem>
#include <utility>

namespace ores::platform::filesystem {

/**
 * @brief A temporary directory that removes itself and its contents.
 *
 * The directory is created empty under the system temporary directory
 * and its whole tree is removed when the guard leaves scope. The
 * destructor never throws, so the guard is safe to hold across an
 * unrelated exception.
 */
class ORES_PLATFORM_EXPORT scoped_temp_directory final {
public:
    /**
     * @brief Creates the temporary directory.
     *
     * @throws io_error if no free path could be created.
     */
    scoped_temp_directory();

    scoped_temp_directory(const scoped_temp_directory&) = delete;
    scoped_temp_directory& operator=(const scoped_temp_directory&) = delete;

    scoped_temp_directory(scoped_temp_directory&& other) noexcept
        : path_(std::exchange(other.path_, std::filesystem::path())) {}

    scoped_temp_directory& operator=(scoped_temp_directory&& other) noexcept {
        if (this != &other) {
            remove_now();
            path_ = std::exchange(other.path_, std::filesystem::path());
        }
        return *this;
    }

    /**
     * @brief Removes the directory tree, and never throws.
     */
    ~scoped_temp_directory();

    /**
     * @brief Returns the path of the directory.
     */
    [[nodiscard]] const std::filesystem::path& path() const noexcept {
        return path_;
    }

    /**
     * @brief Hands the path to the caller and stops removing it.
     *
     * @return the path the guard held, which stays on disk.
     */
    std::filesystem::path release() noexcept {
        return std::exchange(path_, std::filesystem::path());
    }

private:
    void remove_now() noexcept;

    std::filesystem::path path_;
};

}

#endif
