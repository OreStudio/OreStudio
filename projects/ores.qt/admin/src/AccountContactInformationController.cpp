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
#include "ores.qt/AccountContactInformationController.hpp"
#include "ores.eventing.api/domain/event_traits.hpp"
#include "ores.iam.api/eventing/account_contact_information_changed_event.hpp"
#include "ores.iam.api/messaging/account_contact_information_protocol.hpp"
#include "ores.qt/AccountContactInformationDetailDialog.hpp"
#include "ores.qt/AccountContactInformationMdiWindow.hpp"
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/HistoryDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/UiPersistence.hpp"
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
constexpr std::string_view accountContactInformation_event_name =
    eventing::domain::event_traits<iam::eventing::account_contact_information_changed_event>::name;
}

AccountContactInformationController::AccountContactInformationController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    ImageCache* imageCache,
    ChangeReasonCache* changeReasonCache,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow,
                       mdiArea,
                       clientManager,
                       username,
                       accountContactInformation_event_name,
                       parent)
    , changeReasonCache_(changeReasonCache)
    , listWindow_(nullptr)
    , listMdiSubWindow_(nullptr) {
    setImageCache(imageCache);

    BOOST_LOG_SEV(lg(), debug) << "AccountContactInformationController created";
}

void AccountContactInformationController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "account_contact_informations");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new AccountContactInformationMdiWindow(clientManager_, username_, imageCache_);

    // Connect signals
    connect(listWindow_,
            &AccountContactInformationMdiWindow::statusChanged,
            this,
            &AccountContactInformationController::statusMessage);
    connect(listWindow_,
            &AccountContactInformationMdiWindow::errorOccurred,
            this,
            &AccountContactInformationController::errorMessage);
    connect(listWindow_,
            &AccountContactInformationMdiWindow::showInformationDetails,
            this,
            &AccountContactInformationController::onShowDetails);
    connect(listWindow_,
            &AccountContactInformationMdiWindow::addNewRequested,
            this,
            &AccountContactInformationController::onAddNewRequested);
    connect(listWindow_,
            &AccountContactInformationMdiWindow::showInformationHistory,
            this,
            &AccountContactInformationController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Account Contact Information");
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
            [self = QPointer<AccountContactInformationController>(this), key]() {
                if (!self)
                    return;
                self->untrack_window(key);
                self->listWindow_ = nullptr;
                self->listMdiSubWindow_ = nullptr;
            });

    BOOST_LOG_SEV(lg(), debug) << "Account Contact Information list window created";
}

void AccountContactInformationController::closeAllWindows() {
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

void AccountContactInformationController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void AccountContactInformationController::onShowDetails(
    const iam::domain::account_contact_information& accountContactInformation) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << accountContactInformation.full_name;
    showDetailWindow(accountContactInformation);
}

void AccountContactInformationController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new account contact information requested";
    showAddWindow();
}

void AccountContactInformationController::openAdd() {
    showAddWindow();
}
void AccountContactInformationController::openAddWithParent(boost::uuids::uuid parentAccountId) {
    showAddWindow(parentAccountId);
}
void AccountContactInformationController::openEdit(
    const iam::domain::account_contact_information& accountContactInformation) {
    showDetailWindow(accountContactInformation);
}
void AccountContactInformationController::openHistory(
    const iam::domain::account_contact_information& accountContactInformation) {
    showHistoryWindow(accountContactInformation);
}

void AccountContactInformationController::onShowHistory(
    const iam::domain::account_contact_information& accountContactInformation) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: "
                               << accountContactInformation.full_name;
    showHistoryWindow(accountContactInformation);
}

void AccountContactInformationController::showAddWindow(boost::uuids::uuid parentAccountId) {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new account contact information";

    auto* detailDialog = new AccountContactInformationDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setImageCache(imageCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    if (!parentAccountId.is_nil()) {
        iam::domain::account_contact_information prefilled;
        prefilled.account_id = parentAccountId;
        detailDialog->setInformation(prefilled);
    }
    detailDialog->setCreateMode(true);

    connect(detailDialog,
            &AccountContactInformationDetailDialog::statusMessage,
            this,
            &AccountContactInformationController::statusMessage);
    connect(detailDialog,
            &AccountContactInformationDetailDialog::errorMessage,
            this,
            &AccountContactInformationController::errorMessage);
    connect(detailDialog,
            &AccountContactInformationDetailDialog::accountContactInformationSaved,
            this,
            [self = QPointer<AccountContactInformationController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info)
                    << "Account Contact Information saved: " << code.toStdString();
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Account Contact Information");
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::PersonAccounts, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void AccountContactInformationController::showDetailWindow(
    const iam::domain::account_contact_information& accountContactInformation) {

    const QString identifier = QString::fromStdString(accountContactInformation.full_name);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: "
                               << accountContactInformation.full_name;

    auto* detailDialog = new AccountContactInformationDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setImageCache(imageCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setInformation(accountContactInformation);

    connect(detailDialog,
            &AccountContactInformationDetailDialog::statusMessage,
            this,
            &AccountContactInformationController::statusMessage);
    connect(detailDialog,
            &AccountContactInformationDetailDialog::errorMessage,
            this,
            &AccountContactInformationController::errorMessage);
    connect(detailDialog,
            &AccountContactInformationDetailDialog::accountContactInformationSaved,
            this,
            [self = QPointer<AccountContactInformationController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info)
                    << "Account Contact Information saved: " << code.toStdString();
                self->handleEntitySaved();
            });
    connect(detailDialog,
            &AccountContactInformationDetailDialog::accountContactInformationDeleted,
            this,
            [self = QPointer<AccountContactInformationController>(this), key](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info)
                    << "Account Contact Information deleted: " << code.toStdString();
                self->handleEntityDeleted();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Account Contact Information: %1").arg(identifier));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::PersonAccounts, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);
    detailWindow->setGeometryKey(key);

    QPointer<AccountContactInformationController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void AccountContactInformationController::showHistoryWindow(
    const iam::domain::account_contact_information& accountContactInformation) {
    const QString code = QString::fromStdString(accountContactInformation.full_name);
    BOOST_LOG_SEV(lg(), info) << "Opening history window for account contact information: "
                              << accountContactInformation.full_name;

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: "
                                  << accountContactInformation.full_name;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: "
                              << accountContactInformation.full_name;

    const QString entityId =
        QString::fromStdString(boost::uuids::to_string(accountContactInformation.id));
    auto* historyDialog =
        new HistoryDialog(std::string(entity_type_of(iam::domain::account_contact_information{})),
                          entityId.toStdString(),
                          clientManager_,
                          mainWindow_);

    connect(historyDialog,
            &HistoryDialog::statusChanged,
            this,
            [self = QPointer<AccountContactInformationController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(historyDialog,
            &HistoryDialog::errorOccurred,
            this,
            [self = QPointer<AccountContactInformationController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });
    connect(historyDialog,
            &HistoryDialog::revertVersionRequested,
            this,
            [self = QPointer<AccountContactInformationController>(this)](
                const QString& /*entityType*/, const QString& entityId, int version) {
                if (!self)
                    return;
                self->onRevertHistoryVersion(entityId, version);
            });
    connect(historyDialog,
            &HistoryDialog::openVersionRequested,
            this,
            [self = QPointer<AccountContactInformationController>(this)](
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
    historyWindow->setWindowTitle(QString("Account Contact Information History: %1").arg(code));
    historyWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));
    connect_dialog_close(historyDialog, historyWindow);

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);
    historyWindow->setGeometryKey(windowKey);

    QPointer<AccountContactInformationController> self = this;
    connect(historyWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void AccountContactInformationController::onOpenVersion(
    const iam::domain::account_contact_information& accountContactInformation, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for account contact information: "
                              << accountContactInformation.full_name;

    const QString code = QString::fromStdString(accountContactInformation.full_name);
    const QString windowKey =
        build_window_key("version", QString("%1_v%2").arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new AccountContactInformationDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setImageCache(imageCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setInformation(accountContactInformation);
    detailDialog->setReadOnly(true);

    connect(detailDialog,
            &AccountContactInformationDetailDialog::statusMessage,
            this,
            [self = QPointer<AccountContactInformationController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(detailDialog,
            &AccountContactInformationDetailDialog::errorMessage,
            this,
            [self = QPointer<AccountContactInformationController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Account Contact Information: %1 (Version %2)").arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<AccountContactInformationController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void AccountContactInformationController::fetchAccountContactInformationHistory(
    const QString& entityId,
    std::function<void(
        std::expected<std::vector<iam::domain::account_contact_information>, QString>)> callback) {
    iam::messaging::get_account_contact_information_history_request request;
    request.id = entityId.toStdString();

    using FetchResult =
        std::expected<std::vector<iam::domain::account_contact_information>, QString>;

    QPointer<AccountContactInformationController> self = this;
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

void AccountContactInformationController::onOpenHistoryVersion(const QString& entityId,
                                                               int versionNumber) {
    QPointer<AccountContactInformationController> self = this;
    fetchAccountContactInformationHistory(
        entityId,
        [self, entityId, versionNumber](
            std::expected<std::vector<iam::domain::account_contact_information>, QString> result) {
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

void AccountContactInformationController::onRevertHistoryVersion(const QString& entityId,
                                                                 int versionNumber) {
    QPointer<AccountContactInformationController> self = this;
    fetchAccountContactInformationHistory(
        entityId,
        [self, entityId, versionNumber](
            std::expected<std::vector<iam::domain::account_contact_information>, QString> result) {
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

void AccountContactInformationController::onRevertVersion(
    const iam::domain::account_contact_information& accountContactInformation) {
    BOOST_LOG_SEV(lg(), info) << "Reverting account contact information to version: "
                              << accountContactInformation.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new AccountContactInformationDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setImageCache(imageCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    auto reverted_accountContactInformation = accountContactInformation;
    reverted_accountContactInformation.version = 0;
    detailDialog->setInformation(reverted_accountContactInformation);
    detailDialog->setCreateMode(false);
    detailDialog->markDirty();

    connect(detailDialog,
            &AccountContactInformationDetailDialog::statusMessage,
            this,
            &AccountContactInformationController::statusMessage);
    connect(detailDialog,
            &AccountContactInformationDetailDialog::errorMessage,
            this,
            &AccountContactInformationController::errorMessage);
    connect(detailDialog,
            &AccountContactInformationDetailDialog::accountContactInformationSaved,
            this,
            [self = QPointer<AccountContactInformationController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info)
                    << "Account Contact Information reverted: " << code.toStdString();
                emit self->statusMessage(
                    QString("Account Contact Information '%1' reverted successfully").arg(code));
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Revert Account Contact Information: %1")
            .arg(QString::fromStdString(accountContactInformation.full_name)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(Icon::ArrowRotateCounterclockwise,
                                                               IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* AccountContactInformationController::listWindow() const {
    return listWindow_;
}

void AccountContactInformationController::notifyOpenDialogs(const QStringList& entityIds) {
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
