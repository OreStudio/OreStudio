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
#include "ores.qt/CalendarRuleController.hpp"
#include "ores.eventing.api/domain/event_traits.hpp"
#include "ores.qt/CalendarRuleDetailDialog.hpp"
#include "ores.qt/CalendarRuleMdiWindow.hpp"
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/HistoryDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/UiPersistence.hpp"
#include "ores.refdata.api/eventing/calendar_rule_changed_event.hpp"
#include "ores.refdata.api/messaging/calendar_rule_protocol.hpp"
#include <QFutureWatcher>
#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include <QtConcurrent>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>

namespace ores::qt {

using namespace ores::logging;

namespace {
constexpr std::string_view rule_event_name =
    eventing::domain::event_traits<refdata::eventing::calendar_rule_changed_event>::name;
}

CalendarRuleController::CalendarRuleController(QMainWindow* mainWindow,
                                               QMdiArea* mdiArea,
                                               ClientManager* clientManager,
                                               ChangeReasonCache* changeReasonCache,
                                               const QString& username,
                                               QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username, rule_event_name, parent)
    , changeReasonCache_(changeReasonCache)
    , listWindow_(nullptr)
    , listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "CalendarRuleController created";
}

void CalendarRuleController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "calendar_rules");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new CalendarRuleMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_,
            &CalendarRuleMdiWindow::statusChanged,
            this,
            &CalendarRuleController::statusMessage);
    connect(listWindow_,
            &CalendarRuleMdiWindow::errorOccurred,
            this,
            &CalendarRuleController::errorMessage);
    connect(listWindow_,
            &CalendarRuleMdiWindow::showRuleDetails,
            this,
            &CalendarRuleController::onShowDetails);
    connect(listWindow_,
            &CalendarRuleMdiWindow::addNewRequested,
            this,
            &CalendarRuleController::onAddNewRequested);
    connect(listWindow_,
            &CalendarRuleMdiWindow::showRuleHistory,
            this,
            &CalendarRuleController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Calendar Rules");
    listMdiSubWindow_->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::CalendarClock, IconUtils::DefaultIconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    // Track window
    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);
    listMdiSubWindow_->setGeometryKey(key);
    UiPersistence::restoreMdiGeometry(key, listMdiSubWindow_);

    // Cleanup when closed
    connect(listMdiSubWindow_,
            &QObject::destroyed,
            this,
            [self = QPointer<CalendarRuleController>(this), key]() {
                if (!self)
                    return;
                self->untrack_window(key);
                self->listWindow_ = nullptr;
                self->listMdiSubWindow_ = nullptr;
            });

    BOOST_LOG_SEV(lg(), debug) << "Calendar Rule list window created";
}

void CalendarRuleController::closeAllWindows() {
    BOOST_LOG_SEV(lg(), debug) << "closeAllWindows called";

    // Close all managed windows
    QList<QString> keys = managed_windows_.keys();
    for (const QString& key : keys) {
        if (auto* window = managed_windows_.value(key)) {
            window->close();
        }
    }
    managed_windows_.clear();

    listWindow_ = nullptr;
    listMdiSubWindow_ = nullptr;
}

void CalendarRuleController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void CalendarRuleController::onShowDetails(const refdata::domain::calendar_rule& rule) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << boost::uuids::to_string(rule.id);
    showDetailWindow(rule);
}

void CalendarRuleController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new calendar rule requested";
    showAddWindow();
}


void CalendarRuleController::onShowHistory(const refdata::domain::calendar_rule& rule) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: "
                               << boost::uuids::to_string(rule.id);
    showHistoryWindow(rule);
}

void CalendarRuleController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new calendar rule";

    auto* detailDialog = new CalendarRuleDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog,
            &CalendarRuleDetailDialog::statusMessage,
            this,
            &CalendarRuleController::statusMessage);
    connect(detailDialog,
            &CalendarRuleDetailDialog::errorMessage,
            this,
            &CalendarRuleController::errorMessage);
    connect(detailDialog,
            &CalendarRuleDetailDialog::ruleSaved,
            this,
            [self = QPointer<CalendarRuleController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Calendar Rule saved: " << code.toStdString();
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Calendar Rule");
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::CalendarClock, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void CalendarRuleController::showDetailWindow(const refdata::domain::calendar_rule& rule) {

    const QString identifier = QString::fromStdString(boost::uuids::to_string(rule.id));
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: "
                               << boost::uuids::to_string(rule.id);

    auto* detailDialog = new CalendarRuleDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setRule(rule);

    connect(detailDialog,
            &CalendarRuleDetailDialog::statusMessage,
            this,
            &CalendarRuleController::statusMessage);
    connect(detailDialog,
            &CalendarRuleDetailDialog::errorMessage,
            this,
            &CalendarRuleController::errorMessage);
    connect(detailDialog,
            &CalendarRuleDetailDialog::ruleSaved,
            this,
            [self = QPointer<CalendarRuleController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Calendar Rule saved: " << code.toStdString();
                self->handleEntitySaved();
            });
    connect(detailDialog,
            &CalendarRuleDetailDialog::ruleDeleted,
            this,
            [self = QPointer<CalendarRuleController>(this), key](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Calendar Rule deleted: " << code.toStdString();
                self->handleEntityDeleted();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Calendar Rule: %1").arg(identifier));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::CalendarClock, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);
    detailWindow->setGeometryKey(key);

    QPointer<CalendarRuleController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void CalendarRuleController::showHistoryWindow(const refdata::domain::calendar_rule& rule) {
    const QString code = QString::fromStdString(boost::uuids::to_string(rule.id));
    BOOST_LOG_SEV(lg(), info) << "Opening history window for calendar rule: "
                              << boost::uuids::to_string(rule.id);

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: "
                                  << boost::uuids::to_string(rule.id);
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: "
                              << boost::uuids::to_string(rule.id);

    const QString entityId = QString::fromStdString(boost::uuids::to_string(rule.id));
    auto* historyDialog =
        new HistoryDialog(std::string(entity_type_of(refdata::domain::calendar_rule{})),
                          entityId.toStdString(),
                          clientManager_,
                          mainWindow_);

    connect(historyDialog,
            &HistoryDialog::statusChanged,
            this,
            [self = QPointer<CalendarRuleController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(historyDialog,
            &HistoryDialog::errorOccurred,
            this,
            [self = QPointer<CalendarRuleController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });
    connect(historyDialog,
            &HistoryDialog::revertVersionRequested,
            this,
            [self = QPointer<CalendarRuleController>(this)](
                const QString& /*entityType*/, const QString& entityId, int version) {
                if (!self)
                    return;
                self->onRevertHistoryVersion(entityId, version);
            });
    connect(historyDialog,
            &HistoryDialog::openVersionRequested,
            this,
            [self = QPointer<CalendarRuleController>(this)](
                const QString& /*entityType*/, const QString& entityId, int version) {
                if (!self)
                    return;
                self->onOpenHistoryVersion(entityId, version);
            });

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Calendar Rule History: %1").arg(code));
    historyWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));
    connect_dialog_close(historyDialog, historyWindow);

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);
    historyWindow->setGeometryKey(windowKey);

    QPointer<CalendarRuleController> self = this;
    connect(historyWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void CalendarRuleController::onOpenVersion(const refdata::domain::calendar_rule& rule,
                                           int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for calendar rule: " << boost::uuids::to_string(rule.id);

    const QString code = QString::fromStdString(boost::uuids::to_string(rule.id));
    const QString windowKey =
        build_window_key("version", QString("%1_v%2").arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new CalendarRuleDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setRule(rule);
    detailDialog->setReadOnly(true);

    connect(detailDialog,
            &CalendarRuleDetailDialog::statusMessage,
            this,
            [self = QPointer<CalendarRuleController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(detailDialog,
            &CalendarRuleDetailDialog::errorMessage,
            this,
            [self = QPointer<CalendarRuleController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Calendar Rule: %1 (Version %2)").arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<CalendarRuleController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void CalendarRuleController::fetchCalendarRuleHistory(
    const QString& entityId,
    std::function<void(std::expected<std::vector<refdata::domain::calendar_rule>, QString>)>
        callback) {
    refdata::messaging::get_calendar_rule_history_request request;
    request.id = entityId.toStdString();

    using FetchResult = std::expected<std::vector<refdata::domain::calendar_rule>, QString>;

    QPointer<CalendarRuleController> self = this;
    QPointer<ClientManager> clientManager = clientManager_;
    auto future = QtConcurrent::run([clientManager, request = std::move(request)]() -> FetchResult {
        if (!clientManager || !clientManager->isConnected())
            return std::unexpected(QString("Not connected to server"));
        auto result = clientManager->process_authenticated_request(std::move(request));
        if (!result)
            return std::unexpected(QString::fromStdString(result.error()));
        if (!result->success)
            return std::unexpected(QString::fromStdString(result->message));
        return std::move(result->history);
    });

    auto* watcher = new QFutureWatcher<FetchResult>(this);
    connect(watcher,
            &QFutureWatcher<FetchResult>::finished,
            this,
            [self, watcher, callback = std::move(callback)]() mutable {
                auto result = watcher->result();
                watcher->deleteLater();
                if (!self)
                    return;
                callback(std::move(result));
            });
    watcher->setFuture(future);
}

void CalendarRuleController::onOpenHistoryVersion(const QString& entityId, int versionNumber) {
    QPointer<CalendarRuleController> self = this;
    fetchCalendarRuleHistory(
        entityId,
        [self, entityId, versionNumber](
            std::expected<std::vector<refdata::domain::calendar_rule>, QString> result) {
            if (!self)
                return;
            if (!result) {
                emit self->errorMessage(QString("Failed to load history for '%1': %2")
                                            .arg(entityId)
                                            .arg(result.error()));
                return;
            }
            const auto& history = *result;
            const auto it = std::find_if(history.begin(), history.end(), [&](const auto& v) {
                return v.version == versionNumber;
            });
            if (it == history.end()) {
                emit self->errorMessage(
                    QString("Version %1 not found for '%2'").arg(versionNumber).arg(entityId));
                return;
            }
            self->onOpenVersion(*it, versionNumber);
        });
}

void CalendarRuleController::onRevertHistoryVersion(const QString& entityId, int versionNumber) {
    QPointer<CalendarRuleController> self = this;
    fetchCalendarRuleHistory(
        entityId,
        [self, entityId, versionNumber](
            std::expected<std::vector<refdata::domain::calendar_rule>, QString> result) {
            if (!self)
                return;
            if (!result) {
                emit self->errorMessage(QString("Failed to load history for '%1': %2")
                                            .arg(entityId)
                                            .arg(result.error()));
                return;
            }
            const auto& history = *result;
            const auto it = std::find_if(history.begin(), history.end(), [&](const auto& v) {
                return v.version == versionNumber;
            });
            if (it == history.end()) {
                emit self->errorMessage(
                    QString("Version %1 not found for '%2'").arg(versionNumber).arg(entityId));
                return;
            }
            self->onRevertVersion(*it);
        });
}

void CalendarRuleController::onRevertVersion(const refdata::domain::calendar_rule& rule) {
    BOOST_LOG_SEV(lg(), info) << "Reverting calendar rule to version: " << rule.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new CalendarRuleDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    auto reverted_rule = rule;
    reverted_rule.version = 0;
    detailDialog->setRule(reverted_rule);
    detailDialog->setCreateMode(false);
    detailDialog->markDirty();

    connect(detailDialog,
            &CalendarRuleDetailDialog::statusMessage,
            this,
            &CalendarRuleController::statusMessage);
    connect(detailDialog,
            &CalendarRuleDetailDialog::errorMessage,
            this,
            &CalendarRuleController::errorMessage);
    connect(detailDialog,
            &CalendarRuleDetailDialog::ruleSaved,
            this,
            [self = QPointer<CalendarRuleController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Calendar Rule reverted: " << code.toStdString();
                emit self->statusMessage(
                    QString("Calendar Rule '%1' reverted successfully").arg(code));
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Revert Calendar Rule: %1")
            .arg(QString::fromStdString(boost::uuids::to_string(rule.id))));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(Icon::ArrowRotateCounterclockwise,
                                                               IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* CalendarRuleController::listWindow() const {
    return listWindow_;
}

void CalendarRuleController::notifyOpenDialogs(const QStringList& entityIds) {
    for (auto it = managed_windows_.begin(); it != managed_windows_.end(); ++it) {
        auto* window = it.value();
        if (!window)
            continue;

        if (it.key().startsWith("details.")) {
            if (auto* dialog = qobject_cast<DetailDialogBase*>(window->widget())) {
                if (entityIds.isEmpty() || entityIds.contains(dialog->code())) {
                    dialog->markAsStale();
                }
            }
        } else if (it.key().startsWith("history.")) {
            if (auto* dialog = qobject_cast<HistoryDialogBase*>(window->widget())) {
                if (entityIds.isEmpty() || entityIds.contains(dialog->code())) {
                    dialog->markAsStale();
                }
            }
        }
    }
}

}
