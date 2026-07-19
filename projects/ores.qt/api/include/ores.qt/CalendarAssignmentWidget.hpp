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
#ifndef ORES_QT_CALENDAR_ASSIGNMENT_WIDGET_HPP
#define ORES_QT_CALENDAR_ASSIGNMENT_WIDGET_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/OreCalendarComboBox.hpp"
#include "ores.qt/export.hpp"
#include <QGroupBox>
#include <QListWidget>
#include <QToolButton>
#include <QWidget>
#include <functional>
#include <string>
#include <vector>

namespace ores::qt {

class ImageCache;

/**
 * @brief Reusable widget for editing a text-keyed calendar-assignment
 * junction (e.g. currency_calendars, currency_pair_convention_calendars),
 * embeddable in any detail dialog that owns such a junction.
 *
 * Mirrors AccountRolesWidget's shape (QGroupBox + QListWidget + combo +
 * add/remove QToolButtons, staged pending-adds/removes committed by the
 * parent dialog's Save action) generalised to work against any junction
 * whose right side is =calendar= -- the left-side entity supplies its own
 * load/assign/revoke operations via callbacks, since each entity's NATS
 * messages are distinct types hand-authored on its own protocol (junction
 * codegen deliberately has no service/protocol layer of its own -- see
 * the "Retire legacy codegen profile system" story's Decisions).
 *
 * Changes are staged locally (pendingAdds()/pendingRemoves()) and only
 * sent to the server when the parent dialog calls commitChanges(), after
 * it has obtained a change reason/commentary from the user the same way
 * every other write in the app does.
 */
class ORES_QT_API CalendarAssignmentWidget : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.calendar_assignment_widget";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    struct LoadResult {
        bool success = false;
        std::vector<std::string> calendarCodes;
        QString message;
    };

    struct MutateResult {
        bool success = false;
        QString message;
    };

    /**
     * @brief Synchronous fetch of the calendar codes currently assigned to
     * @p leftKey. Called from within QtConcurrent::run by this widget --
     * must not touch the UI thread.
     */
    using LoadAssignedFn = std::function<LoadResult(ClientManager*, const std::string& leftKey)>;

    /**
     * @brief Synchronous assign/revoke of one calendar for @p leftKey.
     * Called from within QtConcurrent::run by this widget -- must not
     * touch the UI thread.
     */
    using AssignFn = std::function<MutateResult(ClientManager*,
                                                const std::string& leftKey,
                                                const std::string& calendarCode,
                                                const std::string& changeReasonCode,
                                                const std::string& changeCommentary)>;
    using RevokeFn = std::function<
        MutateResult(ClientManager*, const std::string& leftKey, const std::string& calendarCode)>;

    explicit CalendarAssignmentWidget(QWidget* parent = nullptr);
    ~CalendarAssignmentWidget() override = default;

    void setClientManager(ClientManager* clientManager);
    void setImageCache(ImageCache* imageCache);
    void setLeftKey(const std::string& leftKey);

    /**
     * @brief Supplies the left-entity-specific load/assign/revoke
     * operations this widget orchestrates. Must be called before load().
     */
    void setCallbacks(LoadAssignedFn loadAssigned, AssignFn assign, RevokeFn revoke);

    /**
     * @brief Load assigned calendars (via the loadAssigned callback) and
     * the full calendar code list. If leftKey is empty (create mode),
     * only the full calendar list is loaded.
     *
     * A no-op if this widget already has data loaded for the current
     * leftKey and @p force is false -- a lazily-loaded tab that is
     * re-activated (e.g. the user switches away and back) must not
     * silently wipe any staged pendingAdds()/pendingRemoves() by
     * re-fetching server state out from under them. Pass @p force to
     * bypass this and always refetch (e.g. after commitChanges()).
     */
    void load(bool force = false);

    void setReadOnly(bool readOnly);

    [[nodiscard]] bool hasPendingChanges() const;
    [[nodiscard]] const std::vector<std::string>& pendingAdds() const;
    [[nodiscard]] const std::vector<std::string>& pendingRemoves() const;

    /**
     * @brief Sends every staged add/remove to the server (assign/revoke
     * per item, via the callbacks supplied to setCallbacks()), stopping at
     * the first failure. Always reloads afterwards -- on a partial
     * failure this drops the already-applied items from pendingAdds()/
     * pendingRemoves() (they succeeded server-side), leaving only the
     * unattempted remainder staged for a retry. Runs asynchronously;
     * @p onComplete is invoked on the UI thread once the batch completes
     * or stops.
     *
     * @param changeReasonCode Applied to every assign in this batch.
     * @param changeCommentary Applied to every assign in this batch.
     * @param onComplete Called with success=false and the failing item's
     * message if a staged change failed; items after it are not attempted.
     */
    void commitChanges(const std::string& changeReasonCode,
                       const std::string& changeCommentary,
                       std::function<void(bool success, const QString& message)> onComplete);

signals:
    void statusMessage(const QString& message);
    void errorMessage(const QString& title, const QString& message);

    /**
     * @brief Emitted when the staged calendar list changes.
     *
     * The parent dialog connects to this signal to enable its Save button.
     */
    void assignmentsChanged();

    /**
     * @brief Emitted once load() completes successfully.
     */
    void dataLoaded();

private slots:
    void onAddClicked();
    void onRemoveClicked();
    void onSelectionChanged();

private:
    void setupUi();
    void refreshView();
    void updateButtonStates();

    QGroupBox* group_;
    QListWidget* assignedList_;
    OreCalendarComboBox* calendarCombo_;
    QToolButton* addButton_;
    QToolButton* removeButton_;

    ClientManager* clientManager_ = nullptr;
    ImageCache* imageCache_ = nullptr;
    std::string leftKey_;
    bool readOnly_ = false;

    LoadAssignedFn loadAssignedFn_;
    AssignFn assignFn_;
    RevokeFn revokeFn_;

    // DB state
    std::vector<std::string> assignedCodes_;
    std::vector<std::string> allCalendarCodes_;

    // Pending local changes (committed by the parent dialog's Save action)
    std::vector<std::string> pendingAdds_;
    std::vector<std::string> pendingRemoves_;

    // Tracks which leftKey_ load() last fetched data for, so a repeat
    // load() on an already-loaded key is a no-op (see load()'s doc comment).
    bool hasLoadedOnce_ = false;
    std::string loadedForKey_;
};

}

#endif
