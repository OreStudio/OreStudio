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
#include <QPointer>
#include <QSignalBlocker>
#include <QSize>
#include <QString>
#include <functional>
#include <string>
#include <vector>

namespace ores::qt {

class ClientManager;
class ImageCache;

/**
 * @brief The type of flag icons to apply to a combo box.
 */
enum class FlagSource { Currency, Country, BusinessCentre };

/**
 * @brief Flag icon for one currency, or a composited pair icon for two — the
 * one function every currency/currency-pair flag in the app should go
 * through, so a single cell (e.g. a "GBP/USD" cell with no separate base/
 * quote columns) and a single-currency cell (e.g. Currency's own code
 * column, or FX Spot Configs' Base/Quote Currency columns) render
 * identically sized and via the same pipeline.
 *
 * Both the single- and two-code cases build on the exact same
 * ImageCache::getCurrencyFlagIcon() call combo boxes and single-currency
 * cells already use — the pair case just composites two of that call's
 * result side by side, rather than using a different rendering path.
 *
 * @param imageCache   Shared image cache (currency ISO -> image_id mapping).
 * @param isoCode      Currency ISO code (or the base code, for a pair).
 * @param quoteIsoCode Quote currency ISO code; empty (default) for a single
 * flag.
 */
ORES_QT_API QIcon currency_flag_icon(ImageCache& imageCache,
                                     const std::string& isoCode,
                                     const std::string& quoteIsoCode = {});

/**
 * @brief The iconSize a view should use to display a currency_flag_icon()
 * pair icon without distortion.
 *
 * A view with no explicit iconSize reserves a roughly *square* box (Qt's
 * default) and scales whatever icon it's given to fit inside it — for a
 * single flag (already close to square) that looks fine, but a pair icon is
 * roughly twice as wide as it is tall, so it gets squeezed down to fit the
 * square box and reads as squished. Pass this to
 * QAbstractItemView::setIconSize() wherever a pair icon (not a single flag)
 * is shown, e.g. a "GBP/USD"-style cell with no separate base/quote columns.
 *
 * @param flagHeight Height of each flag in device-independent pixels — keep
 * close to whatever height Qt's own default renders a single flag at (so the
 * pair icon reads at the same scale as a single-flag column, not bigger).
 */
ORES_QT_API QSize currency_pair_icon_size(int flagHeight = 16);

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

    // QComboBox does not reliably repaint the closed-box icon for the
    // currently-selected index when its icon changes after the fact (it
    // only reflects the change once the popup is shown or the index is
    // reselected). Force it by cycling the current index without
    // notifying listeners.
    const int current = combo->currentIndex();
    if (current >= 0) {
        const QSignalBlocker blocker(combo);
        combo->setCurrentIndex(-1);
        combo->setCurrentIndex(current);
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

/**
 * @brief Populate a combo box with ISO currency codes and flag icons.
 *
 * The one function every currency-picker combo in the app should go
 * through — fetches the current code list asynchronously, repopulates
 * the combo (preserving the current selection if still present), then
 * applies flag icons via apply_flag_icons(). Re-entrant: a fetch already
 * in flight for @p combo is not duplicated.
 *
 * @param combo             The combo box to populate (no-op if null).
 * @param owner             Object outliving the async operation; also
 * used as the re-entrancy guard (a child QFutureWatcher named after
 * @p combo already existing on it means a fetch is already in flight).
 * @param client_manager    Used to run the fetch; no-op if null or
 * disconnected.
 * @param image_cache       Passed to apply_flag_icons() once populated.
 * @param fallback_selection Evaluated at fetch-completion time (not at
 * call time) to get the selection to restore if the combo had none —
 * e.g. the entity's currently-stored value for this field, which may
 * not be set on the widget yet when setup_currency_combo is called
 * (a controller's setClientManager() call, which triggers this fetch,
 * always runs before its set<Entity>() call, which is what actually
 * populates the widget from the loaded record). Optional; omit for a
 * combo with no entity value to restore (e.g. a filter/picker).
 */
ORES_QT_API void setup_currency_combo(QComboBox* combo,
                                      QObject* owner,
                                      ClientManager* client_manager,
                                      ImageCache* image_cache,
                                      std::function<QString()> fallback_selection = {});

}

#endif
