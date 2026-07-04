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
#ifndef ORES_QT_FLAG_ICON_HELPER_HPP
#define ORES_QT_FLAG_ICON_HELPER_HPP

#include "ores.qt/export.hpp"
#include <QAction>
#include <QComboBox>
#include <QIcon>
#include <QLineEdit>
#include <QObject>
#include <QSize>
#include <string>

namespace ores::qt {

class ImageCache;

/**
 * @brief The type of flag icons to apply to a combo box.
 */
enum class FlagSource { Currency, Country, BusinessCentre };

/**
 * @brief Standard flag size (device-independent pixels) for a single flag in
 * a list/grid cell — the one place to change it for every such view at once.
 */
constexpr int kStandardFlagSize = 22;

/**
 * @brief Composited side-by-side icon for a currency pair (e.g. "GBP/USD"),
 * for a single cell/item with no separate base/quote slots to put one flag
 * each on (contrast: a table with distinct base- and quote-currency columns
 * should put one flag on each via ImageCache::getCurrencyFlagIcon() directly,
 * same as any other single-flag cell — no compositing needed there).
 *
 * @param imageCache   Shared image cache (currency ISO -> image_id mapping).
 * @param baseIsoCode  Base currency ISO code.
 * @param quoteIsoCode Quote currency ISO code.
 * @param flagSize     Height/width of each flag, in device-independent pixels.
 */
ORES_QT_API QIcon pair_flag_icon(ImageCache& imageCache,
                                const std::string& baseIsoCode,
                                const std::string& quoteIsoCode,
                                int flagSize = kStandardFlagSize);

/**
 * @brief The QSize a pair_flag_icon() actually renders at — pass to
 * QAbstractItemView::setIconSize() so the view doesn't downscale it back
 * down to its own (small) default icon size.
 */
ORES_QT_API QSize pair_flag_icon_size(int flagSize = kStandardFlagSize);

/**
 * @brief Apply flag icons to a combo box using the given image cache.
 *
 * Sets the icon for every item in @p combo by dispatching to the
 * appropriate ImageCache getter based on @p source. The Qt style then
 * renders the selected item's icon automatically in both the closed-
 * state display and the open dropdown, for editable and non-editable
 * combo boxes alike.
 *
 * @param combo   The combo box to decorate (no-op if null)
 * @param cache   The image cache (no-op if null)
 * @param source  Which flag type to use
 */
ORES_QT_API void apply_flag_icons(QComboBox* combo, ImageCache* cache, FlagSource source);

/**
 * @brief Wire up a combo box for flag icons and keep them current.
 *
 * Calls apply_flag_icons() immediately, then reconnects on
 * ImageCache::allLoaded() so that icons are refreshed once the full
 * set has been downloaded.
 *
 * @param context  The QObject whose lifetime governs the connections
 * @param combo    The combo box to decorate (no-op if null)
 * @param cache    The image cache (no-op if null)
 * @param source   Which flag type to use
 */
ORES_QT_API void
setup_flag_combo(QObject* context, QComboBox* combo, ImageCache* cache, FlagSource source);

/**
 * @brief Set flag icons on every item in a QComboBox.
 *
 * Iterates each item, resolves its flag icon via @p resolver, and calls
 * setItemIcon(). The Qt style renders the selected item's icon in the
 * combo's closed-state display and in the open dropdown automatically.
 *
 * @tparam Resolver  Callable with signature QIcon(const std::string&)
 * @param combo      The combo box to decorate
 * @param resolver   Maps an item's text to a flag icon
 */
template <typename Resolver>
void set_combo_flag_icons(QComboBox* combo, Resolver&& resolver) {
    for (int i = 0; i < combo->count(); ++i) {
        const std::string code = combo->itemText(i).toStdString();
        combo->setItemIcon(i, resolver(code));
    }
    combo->update();
}

/**
 * @brief Set a leading flag icon on a standalone QLineEdit.
 *
 * Used for plain text-entry fields (e.g. the ISO code field in
 * CurrencyDetailDialog) where the field itself is not a combo box.
 * Manages the QAction lifecycle via @p action_ptr: the previous action
 * is removed and deleted before the new one is inserted.
 *
 * @param edit        The line edit to decorate
 * @param icon        The icon to display (empty icon removes the action)
 * @param action_ptr  Reference to the caller-owned QAction pointer
 */
inline void set_line_edit_flag_icon(QLineEdit* edit, const QIcon& icon, QAction*& action_ptr) {
    if (action_ptr) {
        edit->removeAction(action_ptr);
        delete action_ptr;
        action_ptr = nullptr;
    }
    if (!icon.isNull()) {
        action_ptr = edit->addAction(icon, QLineEdit::LeadingPosition);
    }
}

}

#endif
