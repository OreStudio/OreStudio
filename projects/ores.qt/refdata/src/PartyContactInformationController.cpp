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
#include "ores.qt/PartyContactInformationController.hpp"
#include "ores.eventing.api/domain/event_traits.hpp"
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/PartyContactInformationDetailDialog.hpp"
#include "ores.qt/PartyContactInformationHistoryDialog.hpp"
#include "ores.qt/PartyContactInformationMdiWindow.hpp"
#include "ores.qt/UiPersistence.hpp"
#include "ores.refdata.api/eventing/party_contact_information_changed_event.hpp"
#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>

namespace ores::qt {

using namespace ores::logging;

namespace {
constexpr std::string_view partyContactInformation_event_name = eventing::domain::event_traits<
    refdata::eventing::party_contact_information_changed_event>::name;
}

PartyContactInformationController::PartyContactInformationController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    ImageCache* imageCache,
    ChangeReasonCache* changeReasonCache,
    const QString& username,
    QObject* parent)
    : EntityController(
          mainWindow, mdiArea, clientManager, username, partyContactInformation_event_name, parent)
    , changeReasonCache_(changeReasonCache)
    , listWindow_(nullptr)
    , listMdiSubWindow_(nullptr) {
    setImageCache(imageCache);

    BOOST_LOG_SEV(lg(), debug) << "PartyContactInformationController created";
}

void PartyContactInformationController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "party_contact_informations");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new PartyContactInformationMdiWindow(clientManager_, username_, imageCache_);

    // Connect signals
    connect(listWindow_,
            &PartyContactInformationMdiWindow::statusChanged,
            this,
            &PartyContactInformationController::statusMessage);
    connect(listWindow_,
            &PartyContactInformationMdiWindow::errorOccurred,
            this,
            &PartyContactInformationController::errorMessage);
    connect(listWindow_,
            &PartyContactInformationMdiWindow::showInformationDetails,
            this,
            &PartyContactInformationController::onShowDetails);
    connect(listWindow_,
            &PartyContactInformationMdiWindow::addNewRequested,
            this,
            &PartyContactInformationController::onAddNewRequested);
    connect(listWindow_,
            &PartyContactInformationMdiWindow::showInformationHistory,
            this,
            &PartyContactInformationController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Party Contact Information");
    listMdiSubWindow_->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::PersonAccounts, IconUtils::DefaultIconColor));
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
            [self = QPointer<PartyContactInformationController>(this), key]() {
                if (!self)
                    return;
                self->untrack_window(key);
                self->listWindow_ = nullptr;
                self->listMdiSubWindow_ = nullptr;
            });

    BOOST_LOG_SEV(lg(), debug) << "Party Contact Information list window created";
}

void PartyContactInformationController::closeAllWindows() {
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

void PartyContactInformationController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void PartyContactInformationController::onShowDetails(
    const refdata::domain::party_contact_information& partyContactInformation) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << partyContactInformation.contact_type;
    showDetailWindow(partyContactInformation);
}

void PartyContactInformationController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new party contact information requested";
    showAddWindow();
}


void PartyContactInformationController::onShowHistory(
    const refdata::domain::party_contact_information& partyContactInformation) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: "
                               << partyContactInformation.contact_type;
    showHistoryWindow(partyContactInformation);
}

void PartyContactInformationController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new party contact information";

    auto* detailDialog = new PartyContactInformationDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setImageCache(imageCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog,
            &PartyContactInformationDetailDialog::statusMessage,
            this,
            &PartyContactInformationController::statusMessage);
    connect(detailDialog,
            &PartyContactInformationDetailDialog::errorMessage,
            this,
            &PartyContactInformationController::errorMessage);
    connect(detailDialog,
            &PartyContactInformationDetailDialog::partyContactInformationSaved,
            this,
            [self = QPointer<PartyContactInformationController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info)
                    << "Party Contact Information saved: " << code.toStdString();
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Party Contact Information");
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::PersonAccounts, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void PartyContactInformationController::showDetailWindow(
    const refdata::domain::party_contact_information& partyContactInformation) {

    const QString identifier = QString::fromStdString(partyContactInformation.contact_type);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: "
                               << partyContactInformation.contact_type;

    auto* detailDialog = new PartyContactInformationDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setImageCache(imageCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setInformation(partyContactInformation);

    connect(detailDialog,
            &PartyContactInformationDetailDialog::statusMessage,
            this,
            &PartyContactInformationController::statusMessage);
    connect(detailDialog,
            &PartyContactInformationDetailDialog::errorMessage,
            this,
            &PartyContactInformationController::errorMessage);
    connect(detailDialog,
            &PartyContactInformationDetailDialog::partyContactInformationSaved,
            this,
            [self = QPointer<PartyContactInformationController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info)
                    << "Party Contact Information saved: " << code.toStdString();
                self->handleEntitySaved();
            });
    connect(detailDialog,
            &PartyContactInformationDetailDialog::partyContactInformationDeleted,
            this,
            [self = QPointer<PartyContactInformationController>(this), key](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info)
                    << "Party Contact Information deleted: " << code.toStdString();
                self->handleEntityDeleted();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Party Contact Information: %1").arg(identifier));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::PersonAccounts, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);
    detailWindow->setGeometryKey(key);

    QPointer<PartyContactInformationController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void PartyContactInformationController::showHistoryWindow(
    const refdata::domain::party_contact_information& partyContactInformation) {
    const QString code = QString::fromStdString(partyContactInformation.contact_type);
    BOOST_LOG_SEV(lg(), info) << "Opening history window for party contact information: "
                              << partyContactInformation.contact_type;

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: "
                                  << partyContactInformation.contact_type;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: "
                              << partyContactInformation.contact_type;

    auto* historyDialog = new PartyContactInformationHistoryDialog(
        partyContactInformation.id, code, clientManager_, mainWindow_);

    connect(historyDialog,
            &PartyContactInformationHistoryDialog::statusChanged,
            this,
            [self = QPointer<PartyContactInformationController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(historyDialog,
            &PartyContactInformationHistoryDialog::errorOccurred,
            this,
            [self = QPointer<PartyContactInformationController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });
    connect(historyDialog,
            &PartyContactInformationHistoryDialog::revertVersionRequested,
            this,
            &PartyContactInformationController::onRevertVersion);
    connect(historyDialog,
            &PartyContactInformationHistoryDialog::openVersionRequested,
            this,
            &PartyContactInformationController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Party Contact Information History: %1").arg(code));
    historyWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));
    connect_dialog_close(historyDialog, historyWindow);

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);
    historyWindow->setGeometryKey(windowKey);

    QPointer<PartyContactInformationController> self = this;
    connect(historyWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void PartyContactInformationController::onOpenVersion(
    const refdata::domain::party_contact_information& partyContactInformation, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for party contact information: "
                              << partyContactInformation.contact_type;

    const QString code = QString::fromStdString(partyContactInformation.contact_type);
    const QString windowKey =
        build_window_key("version", QString("%1_v%2").arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new PartyContactInformationDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setImageCache(imageCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setInformation(partyContactInformation);
    detailDialog->setReadOnly(true);

    connect(detailDialog,
            &PartyContactInformationDetailDialog::statusMessage,
            this,
            [self = QPointer<PartyContactInformationController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(detailDialog,
            &PartyContactInformationDetailDialog::errorMessage,
            this,
            [self = QPointer<PartyContactInformationController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Party Contact Information: %1 (Version %2)").arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<PartyContactInformationController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void PartyContactInformationController::onRevertVersion(
    const refdata::domain::party_contact_information& partyContactInformation) {
    BOOST_LOG_SEV(lg(), info) << "Reverting party contact information to version: "
                              << partyContactInformation.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new PartyContactInformationDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setImageCache(imageCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    auto reverted_partyContactInformation = partyContactInformation;
    reverted_partyContactInformation.version = 0;
    detailDialog->setInformation(reverted_partyContactInformation);
    detailDialog->setCreateMode(false);
    detailDialog->markDirty();

    connect(detailDialog,
            &PartyContactInformationDetailDialog::statusMessage,
            this,
            &PartyContactInformationController::statusMessage);
    connect(detailDialog,
            &PartyContactInformationDetailDialog::errorMessage,
            this,
            &PartyContactInformationController::errorMessage);
    connect(detailDialog,
            &PartyContactInformationDetailDialog::partyContactInformationSaved,
            this,
            [self = QPointer<PartyContactInformationController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info)
                    << "Party Contact Information reverted: " << code.toStdString();
                emit self->statusMessage(
                    QString("Party Contact Information '%1' reverted successfully").arg(code));
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Revert Party Contact Information: %1")
            .arg(QString::fromStdString(partyContactInformation.contact_type)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(Icon::ArrowRotateCounterclockwise,
                                                               IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* PartyContactInformationController::listWindow() const {
    return listWindow_;
}

void PartyContactInformationController::notifyOpenDialogs(const QStringList& entityIds) {
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
