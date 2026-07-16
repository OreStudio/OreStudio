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
#include "ores.qt/BadgeDefinitionController.hpp"
#include "ores.dq.api/eventing/badge_definition_changed_event.hpp"
#include "ores.dq/messaging/badge_definition_protocol.hpp"
#include "ores.eventing.api/domain/event_traits.hpp"
#include "ores.qt/BadgeDefinitionDetailDialog.hpp"
#include "ores.qt/BadgeDefinitionMdiWindow.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/HistoryDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/UiPersistence.hpp"
#include <QFutureWatcher>
#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include <QtConcurrent>
#include <algorithm>

namespace ores::qt {

using namespace ores::logging;

namespace {
constexpr std::string_view definition_event_name =
    eventing::domain::event_traits<dq::eventing::badge_definition_changed_event>::name;
}

BadgeDefinitionController::BadgeDefinitionController(QMainWindow* mainWindow,
                                                     QMdiArea* mdiArea,
                                                     ClientManager* clientManager,
                                                     const QString& username,
                                                     QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username, definition_event_name, parent)
    , listWindow_(nullptr)
    , listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "BadgeDefinitionController created";
}

void BadgeDefinitionController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "badge_definitions");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new BadgeDefinitionMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_,
            &BadgeDefinitionMdiWindow::statusChanged,
            this,
            &BadgeDefinitionController::statusMessage);
    connect(listWindow_,
            &BadgeDefinitionMdiWindow::errorOccurred,
            this,
            &BadgeDefinitionController::errorMessage);
    connect(listWindow_,
            &BadgeDefinitionMdiWindow::showDefinitionDetails,
            this,
            &BadgeDefinitionController::onShowDetails);
    connect(listWindow_,
            &BadgeDefinitionMdiWindow::addNewRequested,
            this,
            &BadgeDefinitionController::onAddNewRequested);
    connect(listWindow_,
            &BadgeDefinitionMdiWindow::showDefinitionHistory,
            this,
            &BadgeDefinitionController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Badge Definitions");
    listMdiSubWindow_->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Tag, IconUtils::DefaultIconColor));
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
            [self = QPointer<BadgeDefinitionController>(this), key]() {
                if (!self)
                    return;
                self->untrack_window(key);
                self->listWindow_ = nullptr;
                self->listMdiSubWindow_ = nullptr;
            });

    BOOST_LOG_SEV(lg(), debug) << "Badge Definition list window created";
}

void BadgeDefinitionController::closeAllWindows() {
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

void BadgeDefinitionController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void BadgeDefinitionController::onShowDetails(const dq::domain::badge_definition& definition) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << definition.code;
    showDetailWindow(definition);
}

void BadgeDefinitionController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new badge definition requested";
    showAddWindow();
}


void BadgeDefinitionController::onShowHistory(const dq::domain::badge_definition& definition) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << definition.code;
    showHistoryWindow(QString::fromStdString(definition.code));
}

void BadgeDefinitionController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new badge definition";

    auto* detailDialog = new BadgeDefinitionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog,
            &BadgeDefinitionDetailDialog::statusMessage,
            this,
            &BadgeDefinitionController::statusMessage);
    connect(detailDialog,
            &BadgeDefinitionDetailDialog::errorMessage,
            this,
            &BadgeDefinitionController::errorMessage);
    connect(detailDialog,
            &BadgeDefinitionDetailDialog::definitionSaved,
            this,
            [self = QPointer<BadgeDefinitionController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Badge Definition saved: " << code.toStdString();
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Badge Definition");
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Tag, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void BadgeDefinitionController::showDetailWindow(const dq::domain::badge_definition& definition) {

    const QString identifier = QString::fromStdString(definition.code);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << definition.code;

    auto* detailDialog = new BadgeDefinitionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setDefinition(definition);

    connect(detailDialog,
            &BadgeDefinitionDetailDialog::statusMessage,
            this,
            &BadgeDefinitionController::statusMessage);
    connect(detailDialog,
            &BadgeDefinitionDetailDialog::errorMessage,
            this,
            &BadgeDefinitionController::errorMessage);
    connect(detailDialog,
            &BadgeDefinitionDetailDialog::definitionSaved,
            this,
            [self = QPointer<BadgeDefinitionController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Badge Definition saved: " << code.toStdString();
                self->handleEntitySaved();
            });
    connect(detailDialog,
            &BadgeDefinitionDetailDialog::definitionDeleted,
            this,
            [self = QPointer<BadgeDefinitionController>(this), key](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Badge Definition deleted: " << code.toStdString();
                self->handleEntityDeleted();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Badge Definition: %1").arg(identifier));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Tag, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);
    detailWindow->setGeometryKey(key);

    QPointer<BadgeDefinitionController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void BadgeDefinitionController::showHistoryWindow(const QString& code) {
    BOOST_LOG_SEV(lg(), info) << "Opening history window for badge definition: "
                              << code.toStdString();

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: " << code.toStdString();
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: " << code.toStdString();

    auto* historyDialog =
        new HistoryDialog(std::string(entity_type_of(dq::domain::badge_definition{})),
                          code.toStdString(),
                          clientManager_,
                          mainWindow_);

    connect(historyDialog,
            &HistoryDialog::statusChanged,
            this,
            [self = QPointer<BadgeDefinitionController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(historyDialog,
            &HistoryDialog::errorOccurred,
            this,
            [self = QPointer<BadgeDefinitionController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });
    connect(historyDialog,
            &HistoryDialog::revertVersionRequested,
            this,
            [self = QPointer<BadgeDefinitionController>(this)](
                const QString& /*entityType*/, const QString& entityId, int version) {
                if (!self)
                    return;
                self->onRevertHistoryVersion(entityId, version);
            });
    connect(historyDialog,
            &HistoryDialog::openVersionRequested,
            this,
            [self = QPointer<BadgeDefinitionController>(this)](
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
    historyWindow->setWindowTitle(QString("Badge Definition History: %1").arg(code));
    historyWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));
    connect_dialog_close(historyDialog, historyWindow);

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);
    historyWindow->setGeometryKey(windowKey);

    QPointer<BadgeDefinitionController> self = this;
    connect(historyWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void BadgeDefinitionController::onOpenVersion(const dq::domain::badge_definition& definition,
                                              int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for badge definition: " << definition.code;

    const QString code = QString::fromStdString(definition.code);
    const QString windowKey =
        build_window_key("version", QString("%1_v%2").arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new BadgeDefinitionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setDefinition(definition);
    detailDialog->setReadOnly(true);

    connect(detailDialog,
            &BadgeDefinitionDetailDialog::statusMessage,
            this,
            [self = QPointer<BadgeDefinitionController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(detailDialog,
            &BadgeDefinitionDetailDialog::errorMessage,
            this,
            [self = QPointer<BadgeDefinitionController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Badge Definition: %1 (Version %2)").arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<BadgeDefinitionController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void BadgeDefinitionController::fetchBadgeDefinitionHistory(
    const QString& entityId,
    std::function<void(std::expected<std::vector<dq::domain::badge_definition>, QString>)>
        callback) {
    dq::messaging::get_badge_definition_history_request request;
    request.code = entityId.toStdString();

    using FetchResult = std::expected<std::vector<dq::domain::badge_definition>, QString>;

    QPointer<BadgeDefinitionController> self = this;
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

void BadgeDefinitionController::onOpenHistoryVersion(const QString& entityId, int versionNumber) {
    QPointer<BadgeDefinitionController> self = this;
    fetchBadgeDefinitionHistory(
        entityId,
        [self, entityId, versionNumber](
            std::expected<std::vector<dq::domain::badge_definition>, QString> result) {
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

void BadgeDefinitionController::onRevertHistoryVersion(const QString& entityId, int versionNumber) {
    QPointer<BadgeDefinitionController> self = this;
    fetchBadgeDefinitionHistory(
        entityId,
        [self, entityId, versionNumber](
            std::expected<std::vector<dq::domain::badge_definition>, QString> result) {
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

void BadgeDefinitionController::onRevertVersion(const dq::domain::badge_definition& definition) {
    BOOST_LOG_SEV(lg(), info) << "Reverting badge definition to version: " << definition.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new BadgeDefinitionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    auto reverted_definition = definition;
    reverted_definition.version = 0;
    detailDialog->setDefinition(reverted_definition);
    detailDialog->setCreateMode(false);
    detailDialog->markDirty();

    connect(detailDialog,
            &BadgeDefinitionDetailDialog::statusMessage,
            this,
            &BadgeDefinitionController::statusMessage);
    connect(detailDialog,
            &BadgeDefinitionDetailDialog::errorMessage,
            this,
            &BadgeDefinitionController::errorMessage);
    connect(detailDialog,
            &BadgeDefinitionDetailDialog::definitionSaved,
            this,
            [self = QPointer<BadgeDefinitionController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Badge Definition reverted: " << code.toStdString();
                emit self->statusMessage(
                    QString("Badge Definition '%1' reverted successfully").arg(code));
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Revert Badge Definition: %1").arg(QString::fromStdString(definition.code)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(Icon::ArrowRotateCounterclockwise,
                                                               IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* BadgeDefinitionController::listWindow() const {
    return listWindow_;
}

void BadgeDefinitionController::notifyOpenDialogs(const QStringList& entityIds) {
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
