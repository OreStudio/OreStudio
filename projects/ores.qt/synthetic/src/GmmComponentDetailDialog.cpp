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
#include "ores.qt/GmmComponentDetailDialog.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.synthetic.api/messaging/fx_spot_generation_config_protocol.hpp"
#include "ores.synthetic.api/messaging/gmm_component_protocol.hpp"
#include "ui_GmmComponentDetailDialog.h"
#include <QComboBox>
#include <QDoubleSpinBox>
#include <QFutureWatcher>
#include <QMessageBox>
#include <QSpinBox>
#include <QtConcurrent>
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

GmmComponentDetailDialog::GmmComponentDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::GmmComponentDetailDialog)
    , clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
}

GmmComponentDetailDialog::~GmmComponentDetailDialog() {
    delete ui_;
}

QTabWidget* GmmComponentDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* GmmComponentDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* GmmComponentDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

void GmmComponentDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void GmmComponentDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this, &GmmComponentDetailDialog::onSaveClicked);
    connect(
        ui_->closeButton, &QPushButton::clicked, this, &GmmComponentDetailDialog::onCloseClicked);

    connect(ui_->fxSpotConfigCombo,
            &QComboBox::currentIndexChanged,
            this,
            &GmmComponentDetailDialog::onFieldChanged);
    connect(ui_->componentIndexSpin,
            &QSpinBox::valueChanged,
            this,
            &GmmComponentDetailDialog::onFieldChanged);
    connect(ui_->meanSpin,
            &QDoubleSpinBox::valueChanged,
            this,
            &GmmComponentDetailDialog::onFieldChanged);
    connect(ui_->stdevSpin,
            &QDoubleSpinBox::valueChanged,
            this,
            &GmmComponentDetailDialog::onFieldChanged);
    connect(ui_->weightSpin,
            &QDoubleSpinBox::valueChanged,
            this,
            &GmmComponentDetailDialog::onFieldChanged);
}

void GmmComponentDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
    populateFxSpotConfigCombo();
}

void GmmComponentDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void GmmComponentDetailDialog::setComponent(const synthetic::domain::gmm_component& component) {
    component_ = component;
    updateUiFromComponent();
}

void GmmComponentDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    updateSaveButtonState();
}

void GmmComponentDetailDialog::populateFxSpotConfigCombo() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        return;
    }

    if (findChild<QFutureWatcherBase*>("fxSpotConfigComboWatcher"))
        return;

    BOOST_LOG_SEV(lg(), debug) << "Populating FX spot config combo";

    QPointer<GmmComponentDetailDialog> self = this;

    struct FetchResult {
        bool success;
        std::vector<synthetic::domain::fx_spot_generation_config> configs;
    };

    QFuture<FetchResult> future = QtConcurrent::run([self]() -> FetchResult {
        if (!self || !self->clientManager_) {
            return {false, {}};
        }

        synthetic::messaging::get_fx_spot_generation_configs_request request;
        auto response_result =
            self->clientManager_->process_authenticated_request(std::move(request));
        if (!response_result) {
            return {false, {}};
        }

        return {true, std::move(response_result->configs)};
    });

    auto* watcher = new QFutureWatcher<FetchResult>(self);
    watcher->setObjectName("fxSpotConfigComboWatcher");
    connect(watcher, &QFutureWatcher<FetchResult>::finished, self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (!self)
            return;

        if (!result.success) {
            BOOST_LOG_SEV(lg(), error) << "Failed to fetch FX spot configs for combo box.";
            emit self->errorMessage(tr("Could not load FX spot configs."));
            return;
        }

        self->ui_->fxSpotConfigCombo->blockSignals(true);
        self->ui_->fxSpotConfigCombo->clear();

        for (const auto& config : result.configs) {
            const QString id = QString::fromStdString(boost::uuids::to_string(config.id));
            self->ui_->fxSpotConfigCombo->addItem(QString::fromStdString(config.source_name), id);
        }

        // Reselect the currently configured parent, if any.
        const QString currentId =
            QString::fromStdString(boost::uuids::to_string(self->component_.fx_spot_config_id));
        const int idx = self->ui_->fxSpotConfigCombo->findData(currentId);
        if (idx >= 0)
            self->ui_->fxSpotConfigCombo->setCurrentIndex(idx);

        self->ui_->fxSpotConfigCombo->blockSignals(false);

        BOOST_LOG_SEV(lg(), debug)
            << "FX spot config combo populated with " << result.configs.size() << " entries";
    });

    watcher->setFuture(future);
}

void GmmComponentDetailDialog::updateUiFromComponent() {
    {
        const QString currentId =
            QString::fromStdString(boost::uuids::to_string(component_.fx_spot_config_id));
        const int idx = ui_->fxSpotConfigCombo->findData(currentId);
        if (idx >= 0)
            ui_->fxSpotConfigCombo->setCurrentIndex(idx);
    }
    ui_->componentIndexSpin->setValue(component_.component_index);
    ui_->meanSpin->setValue(component_.mean);
    ui_->stdevSpin->setValue(component_.stdev);
    ui_->weightSpin->setValue(component_.weight);

    populateProvenance(component_.version,
                       component_.modified_by,
                       component_.performed_by,
                       component_.recorded_at,
                       component_.change_reason_code,
                       component_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void GmmComponentDetailDialog::updateComponentFromUi() {
    const QString parentId = ui_->fxSpotConfigCombo->currentData().toString();
    if (!parentId.isEmpty()) {
        component_.fx_spot_config_id =
            boost::lexical_cast<boost::uuids::uuid>(parentId.toStdString());
    }
    component_.component_index = ui_->componentIndexSpin->value();
    component_.mean = ui_->meanSpin->value();
    component_.stdev = ui_->stdevSpin->value();
    component_.weight = ui_->weightSpin->value();
    component_.modified_by = username_;
}

void GmmComponentDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void GmmComponentDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput();
    ui_->saveButton->setEnabled(canSave);
}

bool GmmComponentDetailDialog::validateInput() {
    const bool hasParent = !ui_->fxSpotConfigCombo->currentData().toString().isEmpty();
    return hasParent;
}

void GmmComponentDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot save GMM component while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input", "Please fill in all required fields.");
        return;
    }

    const auto crOpType = createMode_ ? ChangeReasonDialog::OperationType::Create :
                                        ChangeReasonDialog::OperationType::Amend;
    const auto crSel = promptChangeReason(crOpType, hasChanges_, createMode_ ? "system" : "common");
    if (!crSel)
        return;
    component_.change_reason_code = crSel->reason_code;
    component_.change_commentary = crSel->commentary;

    updateComponentFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving GMM component index: " << component_.component_index;

    QPointer<GmmComponentDetailDialog> self = this;
    const bool wasCreate = createMode_;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, component = component_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        auto request = synthetic::messaging::save_gmm_component_request::from(component);
        auto response_result =
            self->clientManager_->process_authenticated_request(std::move(request));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(self);
    connect(watcher, &QFutureWatcher<SaveResult>::finished, self, [self, watcher, wasCreate]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "GMM Component saved successfully";
            QString id = QString::fromStdString(boost::uuids::to_string(self->component_.id));
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            if (wasCreate) {
                emit self->gmmComponentCreated(id);
            } else {
                emit self->gmmComponentUpdated(id);
            }
            self->notifySaveSuccess(
                tr("GMM Component %1 saved").arg(self->component_.component_index));
        } else {
            BOOST_LOG_SEV(lg(), error) << "Save failed: " << result.message;
            QString errorMsg = QString::fromStdString(result.message);
            emit self->errorMessage(errorMsg);
            MessageBoxHelper::critical(self, "Save Failed", errorMsg);
        }
    });

    QFuture<SaveResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

}
