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
#include "ores.qt/FxSpotGenerationConfigController.hpp"
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/FxSpotGenerationConfigDetailDialog.hpp"
#include "ores.qt/FxSpotGenerationConfigMdiWindow.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/UiPersistence.hpp"
#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

FxSpotGenerationConfigController::FxSpotGenerationConfigController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    ChangeReasonCache* changeReasonCache,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username, std::string_view{}, parent)
    , listWindow_(nullptr)
    , listMdiSubWindow_(nullptr)
    , changeReasonCache_(changeReasonCache) {

    BOOST_LOG_SEV(lg(), debug) << "FxSpotGenerationConfigController created";
}

void FxSpotGenerationConfigController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "fx_spot_generation_configs");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    listWindow_ = new FxSpotGenerationConfigMdiWindow(clientManager_, username_);

    connect(listWindow_,
            &FxSpotGenerationConfigMdiWindow::statusChanged,
            this,
            &FxSpotGenerationConfigController::statusMessage);
    connect(listWindow_,
            &FxSpotGenerationConfigMdiWindow::errorOccurred,
            this,
            &FxSpotGenerationConfigController::errorMessage);
    connect(listWindow_,
            &FxSpotGenerationConfigMdiWindow::showFxSpotConfigDetails,
            this,
            &FxSpotGenerationConfigController::onShowDetails);
    connect(listWindow_,
            &FxSpotGenerationConfigMdiWindow::addNewRequested,
            this,
            &FxSpotGenerationConfigController::onAddNewRequested);

    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("FX Spot Generation Configs");
    listMdiSubWindow_->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Chart, IconUtils::DefaultIconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);
    listMdiSubWindow_->setGeometryKey(key);
    UiPersistence::restoreMdiGeometry(key, listMdiSubWindow_);

    connect(listMdiSubWindow_,
            &QObject::destroyed,
            this,
            [self = QPointer<FxSpotGenerationConfigController>(this), key]() {
                if (!self)
                    return;
                self->untrack_window(key);
                self->listWindow_ = nullptr;
                self->listMdiSubWindow_ = nullptr;
            });

    BOOST_LOG_SEV(lg(), debug) << "FX Spot Generation Config list window created";
}

void FxSpotGenerationConfigController::closeAllWindows() {
    BOOST_LOG_SEV(lg(), debug) << "closeAllWindows called";

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

void FxSpotGenerationConfigController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void FxSpotGenerationConfigController::onShowDetails(
    const synthetic::domain::fx_spot_generation_config& config) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << config.source_name;
    showDetailWindow(config);
}

void FxSpotGenerationConfigController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new FX spot generation config requested";
    showAddWindow();
}

void FxSpotGenerationConfigController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new FX spot generation config";

    auto* detailDialog = new FxSpotGenerationConfigDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog,
            &FxSpotGenerationConfigDetailDialog::statusMessage,
            this,
            &FxSpotGenerationConfigController::statusMessage);
    connect(detailDialog,
            &FxSpotGenerationConfigDetailDialog::errorMessage,
            this,
            &FxSpotGenerationConfigController::errorMessage);
    connect(detailDialog,
            &FxSpotGenerationConfigDetailDialog::fxSpotConfigCreated,
            this,
            [self = QPointer<FxSpotGenerationConfigController>(this)](const QString& id) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info)
                    << "FX Spot Generation Config created: " << id.toStdString();
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New FX Spot Generation Config");
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Chart, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void FxSpotGenerationConfigController::showDetailWindow(
    const synthetic::domain::fx_spot_generation_config& config) {

    const QString identifier = QString::fromStdString(boost::uuids::to_string(config.id));
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << config.source_name;

    auto* detailDialog = new FxSpotGenerationConfigDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setConfig(config);

    connect(detailDialog,
            &FxSpotGenerationConfigDetailDialog::statusMessage,
            this,
            &FxSpotGenerationConfigController::statusMessage);
    connect(detailDialog,
            &FxSpotGenerationConfigDetailDialog::errorMessage,
            this,
            &FxSpotGenerationConfigController::errorMessage);
    connect(detailDialog,
            &FxSpotGenerationConfigDetailDialog::fxSpotConfigUpdated,
            this,
            [self = QPointer<FxSpotGenerationConfigController>(this)](const QString& id) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info)
                    << "FX Spot Generation Config updated: " << id.toStdString();
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("FX Spot Generation Config: %1").arg(QString::fromStdString(config.source_name)));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Chart, IconUtils::DefaultIconColor));

    track_window(key, detailWindow);
    register_detachable_window(detailWindow);
    detailWindow->setGeometryKey(key);
    UiPersistence::restoreMdiGeometry(key, detailWindow);

    QPointer<FxSpotGenerationConfigController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* FxSpotGenerationConfigController::listWindow() const {
    return listWindow_;
}

}
