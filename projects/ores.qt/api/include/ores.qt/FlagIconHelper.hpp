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
#include <QToolButton>
#include <functional>
#include <optional>
#include <string>
#include <vector>

namespace ores::qt {

class ClientManager;
class ImageCache;

/**
 * @brief The type of flag icons to apply to a combo box.
 */
enum class FlagSource { Currency, Country, BusinessCentre, CurrencyPair, Calendar };

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
 * @brief Overload for an optional ISO code (e.g. currency_pair's nullable
 * settlement_currency): an empty icon when unset, otherwise identical to
 * the non-optional overload.
 */
ORES_QT_API QIcon currency_flag_icon(ImageCache& imageCache,
                                     const std::optional<std::string>& isoCode);

/**
 * @brief Flag pair icon from a single "BASE/QUOTE"-style combined code (e.g.
 * currency_pair_convention's pair_code), splitting on '/' and delegating to
 * currency_flag_icon(). Returns an empty icon if the code doesn't contain a
 * '/'.
 */
ORES_QT_API QIcon currency_flag_icon_from_pair_code(ImageCache& imageCache,
                                                    const std::string& pairCode);

/**
 * @brief Flag icon for one business centre code (e.g. "GBLO", "USNY"),
 * delegating to ImageCache::getBusinessCentreFlagIcon() -- the free-function
 * counterpart to currency_flag_icon(), for model-column accessors (e.g.
 * book's RatesCentreCode column) that need the same call shape as
 * currency_flag_icon rather than a direct ImageCache method call.
 *
 * @param imageCache Shared image cache (business centre code -> country ->
 * image_id mapping).
 * @param businessCentreCode Business centre code; an empty icon is returned
 * for an empty code.
 */
ORES_QT_API QIcon business_centre_flag_icon(ImageCache& imageCache,
                                            const std::string& businessCentreCode);

/**
 * @brief Flag icon for one country, keyed by its ISO 3166-1 alpha-2 code —
 * the =icon_columns= accessor counterpart to =currency_flag_icon()= for
 * entities whose flag is derived from a country code they hold (e.g.
 * business_centre's =country_alpha2_code=) rather than their own uploaded
 * =image_id=.
 *
 * @param imageCache Shared image cache (country alpha-2 -> image_id mapping).
 * @param alpha2Code Country ISO 3166-1 alpha-2 code.
 */
ORES_QT_API QIcon country_flag_icon(ImageCache& imageCache, const std::string& alpha2Code);

/**
 * @brief Flag icon for one calendar, keyed by its QuantLib/ORE token —
 * the =icon_columns= accessor counterpart to =currency_flag_icon()= for
 * calendar (e.g. calendar's own =code= column, or any other entity that
 * references a calendar by code). Delegates to
 * ImageCache::getCalendarFlagIcon(), which prefers the calendar's own
 * =image_id= override before falling back to its =country_code='s flag.
 *
 * @param imageCache Shared image cache (calendar code -> image_id/country
 * mapping).
 * @param calendarCode QuantLib/ORE calendar token; an empty icon is
 * returned for an empty code.
 */
ORES_QT_API QIcon calendar_flag_icon(ImageCache& imageCache, const std::string& calendarCode);

/**
 * @brief Standard single-flag height (device-independent pixels) every list
 * view showing a currency/country/business-centre flag should use.
 *
 * A view with no explicit QAbstractItemView::iconSize falls back to a
 * Qt/style-dependent default that isn't guaranteed to match another view's
 * default — which is exactly how a table mixing single-flag columns with a
 * currency_pair_icon() pair-icon column (e.g. Currency Pairs' Base/Quote/
 * Settlement Currency columns next to its Pair column) ends up rendering
 * single flags at a visibly different size than a plain single-flag table
 * (e.g. Currencies' own code column). Every view showing any currency-style
 * flag — single or paired — must call setIconSize() explicitly with a size
 * derived from this same constant (single_flag_icon_size() or
 * currency_pair_icon_size()), never rely on the implicit default.
 */
inline constexpr int standard_flag_height = 16;

/**
 * @brief The iconSize a view should use to display a single
 * currency_flag_icon() (no pair). Pass to QAbstractItemView::setIconSize()
 * wherever a lone flag column is shown, e.g. Currencies' code column.
 */
ORES_QT_API QSize single_flag_icon_size(int flagHeight = standard_flag_height);

/**
 * @brief The iconSize a view should use to display a currency_flag_icon()
 * pair icon without distortion.
 *
 * A pair icon is roughly twice as wide as it is tall, so squeezing it into
 * the same square box a single flag uses reads as squished. Pass this to
 * QAbstractItemView::setIconSize() wherever a pair icon is shown, e.g. a
 * "GBP/USD"-style cell with no separate base/quote columns — or, for a view
 * that mixes pair and single-flag columns (e.g. Currency Pairs), pass this
 * for the whole view so the pair column isn't squished; the single-flag
 * columns still render at @p flagHeight, identical to a single-flag-only
 * view, since both this and single_flag_icon_size() share the same
 * height.
 *
 * @param flagHeight Height of each flag in device-independent pixels — must
 * match whatever height single_flag_icon_size() was called with
 * elsewhere in the app (default standard_flag_height for both), or single
 * flags in this view will render at a different size than single-flag-only
 * views.
 */
ORES_QT_API QSize currency_pair_icon_size(int flagHeight = standard_flag_height);

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
 * @param iconSize Combo iconSize to apply (defaults to
 * single_flag_icon_size()) — override when a specific combo needs a
 * larger, more legible flag than the app-wide default, e.g. a
 * detail-dialog picker with few enough items that extra icon size costs
 * no layout room.
 */
ORES_QT_API void apply_flag_icons(QComboBox* combo,
                                  ImageCache* cache,
                                  FlagSource source,
                                  QSize iconSize = single_flag_icon_size());

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
ORES_QT_API void setup_flag_combo(QObject* context,
                                  QComboBox* combo,
                                  ImageCache* cache,
                                  FlagSource source,
                                  QSize iconSize = single_flag_icon_size());

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
inline void set_line_edit_flag_icon(QLineEdit* edit,
                                    const QIcon& icon,
                                    QAction*& action_ptr,
                                    QSize iconSize = single_flag_icon_size()) {
    if (action_ptr) {
        edit->removeAction(action_ptr);
        delete action_ptr;
        action_ptr = nullptr;
    }
    if (!icon.isNull()) {
        action_ptr = edit->addAction(icon, QLineEdit::LeadingPosition);
        // QLineEdit renders an action via an internal QToolButton, which
        // has its own default icon size unrelated to the QIcon's actual
        // pixmap sizes. setIconSize() is the documented way to influence
        // it; attempts to force the button's frame size directly on top
        // of that didn't move the rendered size in practice — parked,
        // see task B4526411-7FDC-48E8-A346-DA69B5F66BDD.
        if (auto* button = edit->findChild<QToolButton*>())
            button->setIconSize(iconSize);
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
                                      std::function<QString()> fallback_selection = {},
                                      QSize iconSize = single_flag_icon_size());

/**
 * @brief Populate a combo box with QuantLib/ORE calendar codes and flag
 * icons.
 *
 * The reusable calendar-picker combo every screen that needs to select a
 * calendar should go through — fetches the current calendar code list
 * asynchronously, repopulates the combo (preserving the current
 * selection if still present), then applies flag icons via
 * apply_flag_icons(FlagSource::Calendar). Re-entrant: a fetch already in
 * flight for @p combo is not duplicated. See setup_currency_combo()'s
 * doc comment for the parameter semantics this mirrors.
 */
ORES_QT_API void setup_calendar_combo(QComboBox* combo,
                                      QObject* owner,
                                      ClientManager* client_manager,
                                      ImageCache* image_cache,
                                      std::function<QString()> fallback_selection = {},
                                      QSize iconSize = single_flag_icon_size());

}

#endif
