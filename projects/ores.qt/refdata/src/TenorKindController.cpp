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
#include "ores.qt/TenorKindController.hpp"
#include "ores.eventing.api/domain/event_traits.hpp"
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/TenorKindDetailDialog.hpp"
#include "ores.qt/TenorKindHistoryDialog.hpp"
#include "ores.qt/TenorKindMdiWindow.hpp"
#include "ores.qt/UiPersistence.hpp"
#include "ores.refdata.api/eventing/tenor_kind_changed_event.hpp"
#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>

namespace ores::qt {

using namespace ores::logging;

namespace {
constexpr std::string_view kind_event_name =
    eventing::domain::event_traits<refdata::eventing::tenor_kind_changed_event>::name;
}

TenorKindController::TenorKindController(QMainWindow* mainWindow,
                                         QMdiArea* mdiArea,
                                         ClientManager* clientManager,
                                         ChangeReasonCache* changeReasonCache,
                                         const QString& username,
                                         QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username, kind_event_name, parent)
    , changeReasonCache_(changeReasonCache)
    , listWindow_(nullptr)
    , listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "TenorKindController created";
}

void TenorKindController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "tenor_kinds");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new TenorKindMdiWindow(clientManager_, username_);

    // Connect signals
    connect(
        listWindow_, &TenorKindMdiWindow::statusChanged, this, &TenorKindController::statusMessage);
    connect(
        listWindow_, &TenorKindMdiWindow::errorOccurred, this, &TenorKindController::errorMessage);
    connect(listWindow_,
            &TenorKindMdiWindow::showKindDetails,
            this,
            &TenorKindController::onShowDetails);
    connect(listWindow_,
            &TenorKindMdiWindow::addNewRequested,
            this,
            &TenorKindController::onAddNewRequested);
    connect(listWindow_,
            &TenorKindMdiWindow::showKindHistory,
            this,
            &TenorKindController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Tenor Kinds");
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
            [self = QPointer<TenorKindController>(this), key]() {
                if (!self)
                    return;
                self->untrack_window(key);
                self->listWindow_ = nullptr;
                self->listMdiSubWindow_ = nullptr;
            });

    BOOST_LOG_SEV(lg(), debug) << "Tenor Kind list window created";
}

void TenorKindController::closeAllWindows() {
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

void TenorKindController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void TenorKindController::onShowDetails(const refdata::domain::tenor_kind& kind) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << kind.code;
    showDetailWindow(kind);
}

void TenorKindController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new tenor kind requested";
    showAddWindow();
}


void TenorKindController::onShowHistory(const refdata::domain::tenor_kind& kind) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << kind.code;
    showHistoryWindow(QString::fromStdString(kind.code));
}

void TenorKindController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new tenor kind";

    auto* detailDialog = new TenorKindDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog,
            &TenorKindDetailDialog::statusMessage,
            this,
            &TenorKindController::statusMessage);
    connect(detailDialog,
            &TenorKindDetailDialog::errorMessage,
            this,
            &TenorKindController::errorMessage);
    connect(detailDialog,
            &TenorKindDetailDialog::kindSaved,
            this,
            [self = QPointer<TenorKindController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Tenor Kind saved: " << code.toStdString();
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Tenor Kind");
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Tag, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void TenorKindController::showDetailWindow(const refdata::domain::tenor_kind& kind) {

    const QString identifier = QString::fromStdString(kind.code);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << kind.code;

    auto* detailDialog = new TenorKindDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setKind(kind);

    connect(detailDialog,
            &TenorKindDetailDialog::statusMessage,
            this,
            &TenorKindController::statusMessage);
    connect(detailDialog,
            &TenorKindDetailDialog::errorMessage,
            this,
            &TenorKindController::errorMessage);
    connect(detailDialog,
            &TenorKindDetailDialog::kindSaved,
            this,
            [self = QPointer<TenorKindController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Tenor Kind saved: " << code.toStdString();
                self->handleEntitySaved();
            });
    connect(detailDialog,
            &TenorKindDetailDialog::kindDeleted,
            this,
            [self = QPointer<TenorKindController>(this), key](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Tenor Kind deleted: " << code.toStdString();
                self->handleEntityDeleted();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Tenor Kind: %1").arg(identifier));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Tag, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);
    detailWindow->setGeometryKey(key);

    QPointer<TenorKindController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void TenorKindController::showHistoryWindow(const QString& code) {
    BOOST_LOG_SEV(lg(), info) << "Opening history window for tenor kind: " << code.toStdString();

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: " << code.toStdString();
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: " << code.toStdString();

    auto* historyDialog = new TenorKindHistoryDialog(code, clientManager_, mainWindow_);

    connect(historyDialog,
            &TenorKindHistoryDialog::statusChanged,
            this,
            [self = QPointer<TenorKindController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(historyDialog,
            &TenorKindHistoryDialog::errorOccurred,
            this,
            [self = QPointer<TenorKindController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });
    connect(historyDialog,
            &TenorKindHistoryDialog::revertVersionRequested,
            this,
            &TenorKindController::onRevertVersion);
    connect(historyDialog,
            &TenorKindHistoryDialog::openVersionRequested,
            this,
            &TenorKindController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Tenor Kind History: %1").arg(code));
    historyWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));
    connect_dialog_close(historyDialog, historyWindow);

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);
    historyWindow->setGeometryKey(windowKey);

    QPointer<TenorKindController> self = this;
    connect(historyWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void TenorKindController::onOpenVersion(const refdata::domain::tenor_kind& kind,
                                        int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for tenor kind: " << kind.code;

    const QString code = QString::fromStdString(kind.code);
    const QString windowKey =
        build_window_key("version", QString("%1_v%2").arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new TenorKindDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setKind(kind);
    detailDialog->setReadOnly(true);

    connect(detailDialog,
            &TenorKindDetailDialog::statusMessage,
            this,
            [self = QPointer<TenorKindController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(detailDialog,
            &TenorKindDetailDialog::errorMessage,
            this,
            [self = QPointer<TenorKindController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Tenor Kind: %1 (Version %2)").arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<TenorKindController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void TenorKindController::onRevertVersion(const refdata::domain::tenor_kind& kind) {
    BOOST_LOG_SEV(lg(), info) << "Reverting tenor kind to version: " << kind.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new TenorKindDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    auto reverted_kind = kind;
    reverted_kind.version = 0;
    detailDialog->setKind(reverted_kind);
    detailDialog->setCreateMode(false);
    detailDialog->markDirty();

    connect(detailDialog,
            &TenorKindDetailDialog::statusMessage,
            this,
            &TenorKindController::statusMessage);
    connect(detailDialog,
            &TenorKindDetailDialog::errorMessage,
            this,
            &TenorKindController::errorMessage);
    connect(detailDialog,
            &TenorKindDetailDialog::kindSaved,
            this,
            [self = QPointer<TenorKindController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Tenor Kind reverted: " << code.toStdString();
                emit self->statusMessage(
                    QString("Tenor Kind '%1' reverted successfully").arg(code));
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Revert Tenor Kind: %1").arg(QString::fromStdString(kind.code)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(Icon::ArrowRotateCounterclockwise,
                                                               IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* TenorKindController::listWindow() const {
    return listWindow_;
}

void TenorKindController::notifyOpenDialogs(const QStringList& entityIds) {
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
