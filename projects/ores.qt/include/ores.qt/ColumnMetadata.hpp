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
#ifndef ORES_QT_COLUMN_METADATA_HPP
#define ORES_QT_COLUMN_METADATA_HPP

#include <array>
#include <cstddef>
#include <string_view>
#include <vector>
#include <QVector>

namespace ores::qt {

/**
 * @brief Sentinel value for column default width meaning "auto-size to contents".
 *
 * Used in ColumnMetadata::default_width to indicate that the column should
 * be sized using QHeaderView::ResizeToContents rather than a fixed pixel width.
 */
inline constexpr int kColumnWidthAuto = -1;

/**
 * @brief Column rendering style for table delegates.
 */
enum class column_style {
    text_left,          ///< Proportional font, left-aligned (default).
    text_center,        ///< Proportional font, centered.
    mono_left,          ///< Monospace, left-aligned.
    mono_center,        ///< Monospace, centered.
    mono_bold_left,     ///< Monospace bold, left-aligned.
    mono_right,         ///< Monospace, right-aligned.
    mono_bold_center,   ///< Monospace bold, centered.
    icon_centered,      ///< Centered DecorationRole icon (flag columns).
    badge_centered      ///< Coloured badge pill, centered.
};

/**
 * @brief Metadata for a table column.
 *
 * Provides a single source of truth for column configuration including
 * header text, rendering style, visibility, and width. Used by models
 * to define columns in a way that automatically stays in sync with
 * delegate styles and column visibility settings.
 */
struct ColumnMetadata {
    /** Column index (should match the model's Column enum value). */
    int column;
    /** Header text displayed in the table view. */
    std::string_view header;
    /** Rendering style for the column (font, alignment). */
    column_style style;
    /** If true, column is hidden by default. */
    bool hidden_by_default;
    /** Default width in pixels, or kColumnWidthAuto for auto-size. */
    int default_width;
};

/**
 * @brief Builds a QVector of hidden column indices from a metadata array.
 *
 * This function performs no caching; call sites that want a cached result
 * should wrap this in a static local in their own function, ensuring the
 * static is unique per model rather than shared across all models with
 * the same column count.
 *
 * Usage in model header:
 * @code
 *   static QVector<int> defaultHiddenColumns() {
 *       static QVector<int> const result =
 *           ::ores::qt::defaultHiddenColumns<kColumnCount>(kColumns);
 *       return result;
 *   }
 * @endcode
 *
 * @tparam N The number of columns.
 * @param columns The column metadata array.
 * @return QVector of column indices that are hidden by default.
 */
template <std::size_t N>
[[nodiscard]] inline QVector<int>
defaultHiddenColumns(const std::array<ColumnMetadata, N>& columns) {
    QVector<int> result;
    for (std::size_t i = 0; i < N; ++i)
        if (columns[i].hidden_by_default)
            result.push_back(columns[i].column);
    return result;
}

}

#endif
