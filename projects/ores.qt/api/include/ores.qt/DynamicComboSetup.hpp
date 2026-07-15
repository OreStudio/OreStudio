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
#ifndef ORES_QT_DYNAMIC_COMBO_SETUP_HPP
#define ORES_QT_DYNAMIC_COMBO_SETUP_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientManager.hpp"
#include <QComboBox>
#include <QFutureWatcher>
#include <QObject>
#include <QPointer>
#include <QString>
#include <QtConcurrent>
#include <algorithm>
#include <expected>
#include <functional>
#include <vector>

namespace ores::qt {

namespace detail {
[[nodiscard]] inline auto& dynamic_combo_setup_lg() {
    using namespace ores::logging;
    static auto instance = make_logger("ores.qt.dynamic_combo_setup");
    return instance;
}
}

/**
 * @brief Populates a QComboBox asynchronously from a server-fetched
 * reference-data vector, sorted and tooltip-annotated.
 *
 * Generalises the fetch/sort/tooltip/placeholder/restore-selection
 * pattern that grew organically in currency's rounding_type,
 * monetary_nature, and market_tier combos. The caller supplies the
 * per-entity specifics (fetch call and field accessors); this function
 * does the async plumbing, sorting, tooltip wiring, and selection
 * restore that were previously duplicated per field.
 *
 * @param combo The combo box widget to populate.
 * @param owner Object outliving the async operation, used to own the
 * QFutureWatcher and as the guard against overlapping fetches (a
 * QFutureWatcherBase child named watcher_name already existing on it
 * means a fetch is already in flight).
 * @param client_manager Client manager used to run the fetch.
 * @param fetch Synchronous call fetching the full entity vector, or an
 * error message on failure; intended to be one of the LookupFetcher
 * functions, run off the UI thread by this helper. Distinguishing
 * failure from a legitimately-empty result lets the caller surface a
 * fetch error instead of silently rendering an empty combo.
 * @param watcher_name Unique object name for the QFutureWatcher,
 * used both for re-entrancy guarding and lookup.
 * @param code_of Extracts the text shown as each combo item's label.
 * For most callers this also doubles as the stored/matched value (a
 * lookup entity's code IS its display text, e.g. book_status "Active")
 * — pass @p value_of only when the two genuinely differ (e.g. a
 * UUID-keyed parent picker: display the parent's name, store its id).
 * @param tooltip_of Extracts the per-item tooltip text.
 * @param sort_key_of Extracts the sort key (ascending) items are
 * ordered by before insertion.
 * @param fallback_selection Evaluated at fetch-completion time (not at
 * call time) to get the *value* (matched against @p value_of, or @p
 * code_of when @p value_of is unset) to restore if there was no prior
 * selection on the combo — e.g. the current entity's stored value for
 * this field, which may not be set yet when populateDynamicCombo is
 * called (setClientManager runs before setCurrency).
 * @param on_error Called with the error message when the fetch fails;
 * the combo shows a distinct "Failed to load" placeholder in that
 * case. Defaults to a no-op for callers that don't need to surface it
 * beyond the placeholder.
 * @param on_success Called after items are populated and the selection
 * restored, on the success path only. Defaults to a no-op; used by
 * badge-coloured combos to (re)apply per-item badge colours once the
 * real items exist, since a delegate/paintEvent installed before the
 * fetch completes would have nothing to colour yet.
 * @param loading_placeholder Text shown while the fetch is in flight.
 * @param error_placeholder Text shown in the combo when the fetch fails.
 * @param value_of Extracts the value stored as combo item data (@c
 * Qt::UserRole) and used to match/restore selection — distinct from
 * the displayed text when set (e.g. a UUID id backing a name-labelled
 * combo). Defaults to @p code_of when unset, preserving the original
 * text-is-the-value behaviour for every existing caller.
 * @param exclude_if Evaluated on the main thread against each fetched
 * item after the fetch completes; items for which it returns true are
 * dropped before sorting/populating. Used by self-referencing combos
 * (e.g. a party's own parent-party picker) to exclude the entity
 * currently being edited from its own parent list — both to prevent
 * self-parenting and because a self-referencing row breaks a
 * recursive hierarchy CTE (UNION ALL never dedupes a row that matches
 * itself, so it loops forever). Defaults to a no-op (nothing
 * excluded), preserving existing behaviour for every non-self-
 * referencing caller.
 */
template <typename Entity>
void populateDynamicCombo(
    QComboBox* combo,
    QObject* owner,
    ClientManager* client_manager,
    std::function<std::expected<std::vector<Entity>, QString>(ClientManager*)> fetch,
    const QString& watcher_name,
    std::function<QString(const Entity&)> code_of,
    std::function<QString(const Entity&)> tooltip_of,
    std::function<int(const Entity&)> sort_key_of,
    std::function<QString()> fallback_selection,
    std::function<void(const QString&)> on_error = [](const QString&) {},
    std::function<void()> on_success = []() {},
    const QString& loading_placeholder = QObject::tr("Loading…"),
    const QString& error_placeholder = QObject::tr("Failed to load"),
    std::function<QString(const Entity&)> value_of = nullptr,
    std::function<bool(const Entity&)> exclude_if = [](const Entity&) { return false; }) {
    if (!value_of)
        value_of = code_of;
    if (!combo || !owner || !client_manager || !client_manager->isConnected()) {
        BOOST_LOG_SEV(detail::dynamic_combo_setup_lg(), ores::logging::debug)
            << watcher_name.toStdString() << ": early return, combo=" << (combo != nullptr)
            << " owner=" << (owner != nullptr) << " client_manager=" << (client_manager != nullptr)
            << " connected=" << (client_manager && client_manager->isConnected());
        return;
    }

    if (owner->findChild<QFutureWatcherBase*>(watcher_name)) {
        BOOST_LOG_SEV(detail::dynamic_combo_setup_lg(), ores::logging::debug)
            << watcher_name.toStdString() << ": early return, watcher already in flight";
        return;
    }
    BOOST_LOG_SEV(detail::dynamic_combo_setup_lg(), ores::logging::debug)
        << watcher_name.toStdString() << ": starting fetch";

    const QString previous_selection = combo->currentData().toString();

    combo->blockSignals(true);
    combo->clear();
    combo->addItem(loading_placeholder);
    combo->blockSignals(false);

    QPointer<QObject> self = owner;
    using Result = std::expected<std::vector<Entity>, QString>;
    QFuture<Result> future = QtConcurrent::run([self, client_manager, fetch]() -> Result {
        if (!self)
            return std::vector<Entity>{};
        return fetch(client_manager);
    });

    auto* watcher = new QFutureWatcher<Result>(owner);
    watcher->setObjectName(watcher_name);
    QObject::connect(
        watcher,
        &QFutureWatcher<Result>::finished,
        owner,
        [self,
         watcher,
         combo,
         previous_selection,
         fallback_selection,
         code_of,
         value_of,
         tooltip_of,
         sort_key_of,
         exclude_if,
         on_error,
         on_success,
         error_placeholder]() {
            auto result = watcher->result();
            watcher->deleteLater();

            BOOST_LOG_SEV(detail::dynamic_combo_setup_lg(), ores::logging::debug)
                << "fetch finished, self valid=" << (self != nullptr)
                << " has_result=" << result.has_value();

            if (!self)
                return;

            if (!result) {
                BOOST_LOG_SEV(detail::dynamic_combo_setup_lg(), ores::logging::debug)
                    << "fetch error: " << result.error().toStdString();
                combo->blockSignals(true);
                combo->clear();
                combo->addItem(error_placeholder);
                combo->blockSignals(false);
                on_error(result.error());
                return;
            }

            auto items = std::move(*result);
            items.erase(std::remove_if(items.begin(), items.end(), exclude_if), items.end());
            std::sort(items.begin(), items.end(), [&sort_key_of](const auto& a, const auto& b) {
                return sort_key_of(a) < sort_key_of(b);
            });

            combo->blockSignals(true);
            combo->clear();
            for (const auto& item : items) {
                combo->addItem(code_of(item));
                combo->setItemData(combo->count() - 1, value_of(item), Qt::UserRole);
                combo->setItemData(combo->count() - 1, tooltip_of(item), Qt::ToolTipRole);
            }

            const QString to_select =
                !previous_selection.isEmpty() ? previous_selection : fallback_selection();
            if (!to_select.isEmpty()) {
                const int idx = combo->findData(to_select);
                if (idx >= 0)
                    combo->setCurrentIndex(idx);
            }
            combo->blockSignals(false);

            const int current = combo->currentIndex();
            if (current >= 0)
                combo->setToolTip(combo->itemData(current, Qt::ToolTipRole).toString());

            on_success();
        });

    watcher->setFuture(future);
}

}

#endif
