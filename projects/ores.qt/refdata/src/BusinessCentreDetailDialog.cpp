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
#include "ores.qt/BusinessCentreDetailDialog.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/DynamicComboSetup.hpp"
#include "ores.qt/FlagIconHelper.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/LookupFetcher.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.refdata.api/messaging/business_centre_protocol.hpp"
#include "ui_BusinessCentreDetailDialog.h"
#include <QComboBox>
#include <QFutureWatcher>
#include <QMessageBox>
#include <QtConcurrent>
#include <algorithm>

namespace ores::qt {

using namespace ores::logging;

BusinessCentreDetailDialog::BusinessCentreDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::BusinessCentreDetailDialog)
    , clientManager_(nullptr) {

    ui_->setupUi(this);
    WidgetUtils::setupComboBoxes(this);
    setupUi();
    setupCombos();
    setupConnections();
    // Hierarchy tree seam: a future :implements 9B165431-2921-4CAC-A2E8-2C186741E523
    // block is expected to construct a HierarchyModelBuilder-derived model
    // for this entity, wrap it in a HierarchyTreeWidget, and insert that
    // widget into this dialog's layout (e.g. a dedicated tab). Left empty
    // when no entity implements this kind.
    // Composite child-entity tables seam: an :implements
    // 7E4A2C8D-9F1B-4E6A-8D3C-5B2A7E9F1C4D block constructs one QTableWidget
    // + QToolBar per embedded child entity (e.g. identifiers, contact
    // information), wraps each in a tab, and inserts it into this dialog's
    // tab widget. Left empty when no entity implements this kind.
}

BusinessCentreDetailDialog::~BusinessCentreDetailDialog() {
    delete ui_;
}

QTabWidget* BusinessCentreDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* BusinessCentreDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* BusinessCentreDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

QString BusinessCentreDetailDialog::code() const {
    return QString::fromStdString(business_centre_.code);
}

void BusinessCentreDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void BusinessCentreDetailDialog::setupCombos() {}

void BusinessCentreDetailDialog::setupConnections() {
    connect(
        ui_->saveButton, &QPushButton::clicked, this, &BusinessCentreDetailDialog::onSaveClicked);
    connect(ui_->deleteButton,
            &QPushButton::clicked,
            this,
            &BusinessCentreDetailDialog::onDeleteClicked);
    connect(
        ui_->closeButton, &QPushButton::clicked, this, &BusinessCentreDetailDialog::onCloseClicked);

    connect(
        ui_->codeEdit, &QLineEdit::textChanged, this, &BusinessCentreDetailDialog::onCodeChanged);
    connect(ui_->sourceEdit,
            &QLineEdit::textChanged,
            this,
            &BusinessCentreDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit,
            &QLineEdit::textChanged,
            this,
            &BusinessCentreDetailDialog::onFieldChanged);
    connect(ui_->cityNameEdit,
            &QLineEdit::textChanged,
            this,
            &BusinessCentreDetailDialog::onFieldChanged);
    connect(ui_->codingSchemeCombo,
            &QComboBox::currentIndexChanged,
            this,
            &BusinessCentreDetailDialog::onFieldChanged);
    connect(ui_->countryAlpha2Combo,
            &QComboBox::currentIndexChanged,
            this,
            &BusinessCentreDetailDialog::onFieldChanged);
}

void BusinessCentreDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
    populateCodingSchemeCombo();
    populateCountryAlpha2CodeCombo();
}

void BusinessCentreDetailDialog::populateCountryAlpha2CodeCombo() {
    if (!clientManager_ || !clientManager_->isConnected())
        return;

    QPointer<BusinessCentreDetailDialog> self = this;
    auto* watcher = new QFutureWatcher<std::vector<std::string>>(self);
    QObject::connect(
        watcher, &QFutureWatcher<std::vector<std::string>>::finished, self, [self, watcher]() {
            auto codes = watcher->result();
            watcher->deleteLater();
            if (!self)
                return;

            auto* combo = self->ui_->countryAlpha2Combo;
            const QString previous = combo->currentText();
            combo->blockSignals(true);
            combo->clear();
            combo->addItem(QString());
            for (const auto& c : codes)
                combo->addItem(QString::fromStdString(c));
            // fallback_selection is evaluated here (fetch-completion time), not
            // at populate-call time, since setCentre() may run before or
            // after setClientManager() triggers this fetch.
            const QString fallback =
                QString::fromStdString(self->business_centre_.country_alpha2_code);
            const QString to_select = !previous.isEmpty() ? previous : fallback;
            if (!to_select.isEmpty()) {
                const int idx = combo->findText(to_select);
                if (idx >= 0)
                    combo->setCurrentIndex(idx);
            }
            combo->blockSignals(false);

            if (self->imageCache())
                apply_flag_icons(
                    combo, self->imageCache(), FlagSource::Country, single_flag_icon_size());
        });

    auto* cm = clientManager_;
    watcher->setFuture(QtConcurrent::run([cm]() { return fetch_country_codes(cm); }));
}

void BusinessCentreDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void BusinessCentreDetailDialog::setCentre(
    const refdata::domain::business_centre& business_centre) {
    business_centre_ = business_centre;
    updateUiFromCentre();
}

void BusinessCentreDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->cityNameEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    updateSaveButtonState();
}

void BusinessCentreDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void BusinessCentreDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->codeEdit->setReadOnly(true);
    ui_->sourceEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->cityNameEdit->setReadOnly(true);
    ui_->codingSchemeCombo->setEnabled(!readOnly);
    ui_->countryAlpha2Combo->setEnabled(!readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void BusinessCentreDetailDialog::populateCodingSchemeCombo() {
    BOOST_LOG_SEV(lg(), debug) << "Populating coding_scheme_code combo";
    populateDynamicCombo<dq::domain::coding_scheme>(
        ui_->codingSchemeCombo,
        this,
        clientManager_,
        &fetch_coding_schemes,
        "codingSchemeWatcher",
        [](const auto& t) { return QString::fromStdString(t.code); },
        [](const auto& t) { return QString::fromStdString(t.description); },
        [](const auto& t) { return t.version; },
        [this]() { return QString::fromStdString(business_centre_.coding_scheme_code); },
        [this](const QString& error) {
            emit errorMessage(tr("Failed to load coding schemes: %1").arg(error));
        },
        []() {},
        QObject::tr("Loading…"),
        QObject::tr("Failed to load"),
        [](const auto& t) { return QString::fromStdString(t.code); },
        [](const auto&) { return false; },
        QString{});
}
void BusinessCentreDetailDialog::updateUiFromCentre() {
    ui_->codeEdit->setText(QString::fromStdString(business_centre_.code));
    ui_->sourceEdit->setText(QString::fromStdString(business_centre_.source));
    ui_->descriptionEdit->setText(QString::fromStdString(business_centre_.description));
    ui_->cityNameEdit->setText(QString::fromStdString(business_centre_.city_name));
    {
        const auto val = QString::fromStdString(business_centre_.coding_scheme_code);
        const int idx = ui_->codingSchemeCombo->findData(val);
        if (idx >= 0)
            ui_->codingSchemeCombo->setCurrentIndex(idx);
    }
    {
        const auto val = QString::fromStdString(business_centre_.country_alpha2_code);
        const int idx = ui_->countryAlpha2Combo->findText(val);
        ui_->countryAlpha2Combo->setCurrentIndex(idx);
    }

    populateProvenance(business_centre_.version,
                       business_centre_.modified_by,
                       business_centre_.performed_by,
                       business_centre_.recorded_at,
                       business_centre_.change_reason_code,
                       business_centre_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void BusinessCentreDetailDialog::updateCentreFromUi() {
    if (createMode_) {
        business_centre_.code = ui_->codeEdit->text().trimmed().toStdString();
    }
    business_centre_.source = ui_->sourceEdit->text().trimmed().toStdString();
    business_centre_.description = ui_->descriptionEdit->text().trimmed().toStdString();
    if (createMode_) {
        business_centre_.city_name = ui_->cityNameEdit->text().trimmed().toStdString();
    }
    business_centre_.coding_scheme_code = ui_->codingSchemeCombo->currentText().toStdString();
    business_centre_.country_alpha2_code = ui_->countryAlpha2Combo->currentText().toStdString();
    business_centre_.modified_by = username_;
}

void BusinessCentreDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void BusinessCentreDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void BusinessCentreDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool BusinessCentreDetailDialog::validateInput() {
    const QString code_val = ui_->codeEdit->text().trimmed();
    const bool coding_scheme_code_selected = ui_->codingSchemeCombo->currentIndex() >= 0;

    return true && !code_val.isEmpty() && coding_scheme_code_selected;
}

void BusinessCentreDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot save business centre while disconnected from server.");
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
    business_centre_.change_reason_code = crSel->reason_code;
    business_centre_.change_commentary = crSel->commentary;

    updateCentreFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving business centre: " << business_centre_.code;

    QPointer<BusinessCentreDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, business_centre = business_centre_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_business_centre_request request;
        request.data = business_centre;
        auto response_result =
            self->clientManager_->process_authenticated_request(std::move(request));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(self);
    connect(watcher, &QFutureWatcher<SaveResult>::finished, self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Business Centre saved successfully";
            QString code = QString::fromStdString(self->business_centre_.code);
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->business_centreSaved(code);
            self->notifySaveSuccess(tr("Business Centre '%1' saved").arg(code));
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

void BusinessCentreDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot delete business centre while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(business_centre_.code);
    auto reply = MessageBoxHelper::question(
        this,
        "Delete Business Centre",
        QString("Are you sure you want to delete business centre '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel =
        promptChangeReason(ChangeReasonDialog::OperationType::Delete, false, "common");
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting business centre: " << business_centre_.code;

    QPointer<BusinessCentreDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, code = business_centre_.code]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_business_centre_request request;
        request.codes = {code};
        auto response_result =
            self->clientManager_->process_authenticated_request(std::move(request));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished, self, [self, code, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Business Centre deleted successfully";
            emit self->statusMessage(QString("Business Centre '%1' deleted").arg(code));
            emit self->business_centreDeleted(code);
            self->requestClose();
        } else {
            BOOST_LOG_SEV(lg(), error) << "Delete failed: " << result.message;
            QString errorMsg = QString::fromStdString(result.message);
            emit self->errorMessage(errorMsg);
            MessageBoxHelper::critical(self, "Delete Failed", errorMsg);
        }
    });

    QFuture<DeleteResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}


}
