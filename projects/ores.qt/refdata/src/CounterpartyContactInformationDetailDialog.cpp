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
#include "ores.qt/CounterpartyContactInformationDetailDialog.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/DynamicComboSetup.hpp"
#include "ores.qt/FlagIconHelper.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/LookupFetcher.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.refdata.api/messaging/counterparty_contact_information_protocol.hpp"
#include "ui_CounterpartyContactInformationDetailDialog.h"
#include <QComboBox>
#include <QFutureWatcher>
#include <QMessageBox>
#include <QtConcurrent>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>

namespace ores::qt {

using namespace ores::logging;

CounterpartyContactInformationDetailDialog::CounterpartyContactInformationDetailDialog(
    QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::CounterpartyContactInformationDetailDialog)
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

CounterpartyContactInformationDetailDialog::~CounterpartyContactInformationDetailDialog() {
    delete ui_;
}

QTabWidget* CounterpartyContactInformationDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* CounterpartyContactInformationDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* CounterpartyContactInformationDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

QString CounterpartyContactInformationDetailDialog::code() const {
    return QString::fromStdString(counterpartyContactInformation_.contact_type);
}

void CounterpartyContactInformationDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void CounterpartyContactInformationDetailDialog::setupCombos() {}

void CounterpartyContactInformationDetailDialog::setupConnections() {
    connect(ui_->saveButton,
            &QPushButton::clicked,
            this,
            &CounterpartyContactInformationDetailDialog::onSaveClicked);
    connect(ui_->deleteButton,
            &QPushButton::clicked,
            this,
            &CounterpartyContactInformationDetailDialog::onDeleteClicked);
    connect(ui_->closeButton,
            &QPushButton::clicked,
            this,
            &CounterpartyContactInformationDetailDialog::onCloseClicked);

    connect(ui_->idEdit,
            &QLineEdit::textChanged,
            this,
            &CounterpartyContactInformationDetailDialog::onCodeChanged);
    connect(ui_->contactTypeCombo,
            &QComboBox::currentIndexChanged,
            this,
            &CounterpartyContactInformationDetailDialog::onFieldChanged);
    connect(ui_->streetLine1Edit,
            &QLineEdit::textChanged,
            this,
            &CounterpartyContactInformationDetailDialog::onFieldChanged);
    connect(ui_->streetLine2Edit,
            &QLineEdit::textChanged,
            this,
            &CounterpartyContactInformationDetailDialog::onFieldChanged);
    connect(ui_->cityEdit,
            &QLineEdit::textChanged,
            this,
            &CounterpartyContactInformationDetailDialog::onFieldChanged);
    connect(ui_->stateEdit,
            &QLineEdit::textChanged,
            this,
            &CounterpartyContactInformationDetailDialog::onFieldChanged);
    connect(ui_->countryCodeCombo,
            &QComboBox::currentIndexChanged,
            this,
            &CounterpartyContactInformationDetailDialog::onFieldChanged);
    connect(ui_->postalCodeEdit,
            &QLineEdit::textChanged,
            this,
            &CounterpartyContactInformationDetailDialog::onFieldChanged);
    connect(ui_->phoneEdit,
            &QLineEdit::textChanged,
            this,
            &CounterpartyContactInformationDetailDialog::onFieldChanged);
    connect(ui_->emailEdit,
            &QLineEdit::textChanged,
            this,
            &CounterpartyContactInformationDetailDialog::onFieldChanged);
    connect(ui_->webPageEdit,
            &QLineEdit::textChanged,
            this,
            &CounterpartyContactInformationDetailDialog::onFieldChanged);
}

void CounterpartyContactInformationDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
    populateContactTypeCombo();
    populateCountryCodeCombo();
}

void CounterpartyContactInformationDetailDialog::populateCountryCodeCombo() {
    if (!clientManager_ || !clientManager_->isConnected())
        return;

    QPointer<CounterpartyContactInformationDetailDialog> self = this;
    auto* watcher = new QFutureWatcher<std::vector<std::string>>(self);
    QObject::connect(
        watcher, &QFutureWatcher<std::vector<std::string>>::finished, self, [self, watcher]() {
            auto codes = watcher->result();
            watcher->deleteLater();
            if (!self)
                return;

            auto* combo = self->ui_->countryCodeCombo;
            const QString previous = combo->currentText();
            combo->blockSignals(true);
            combo->clear();
            combo->addItem(QString());
            for (const auto& c : codes)
                combo->addItem(QString::fromStdString(c));
            // fallback_selection is evaluated here (fetch-completion time), not
            // at populate-call time, since setInformation() may run before or
            // after setClientManager() triggers this fetch.
            const QString fallback =
                QString::fromStdString(self->counterpartyContactInformation_.country_code);
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

void CounterpartyContactInformationDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void CounterpartyContactInformationDetailDialog::setInformation(
    const refdata::domain::counterparty_contact_information& counterpartyContactInformation) {
    counterpartyContactInformation_ = counterpartyContactInformation;
    updateUiFromInformation();
}

void CounterpartyContactInformationDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->idEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    if (createMode) {
        counterpartyContactInformation_.id = boost::uuids::random_generator()();
    }
    hasChanges_ = false;
    updateSaveButtonState();
}

void CounterpartyContactInformationDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CounterpartyContactInformationDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->idEdit->setReadOnly(true);
    ui_->contactTypeCombo->setEnabled(!readOnly);
    ui_->streetLine1Edit->setReadOnly(readOnly);
    ui_->streetLine2Edit->setReadOnly(readOnly);
    ui_->cityEdit->setReadOnly(readOnly);
    ui_->stateEdit->setReadOnly(readOnly);
    ui_->countryCodeCombo->setEnabled(!readOnly);
    ui_->postalCodeEdit->setReadOnly(readOnly);
    ui_->phoneEdit->setReadOnly(readOnly);
    ui_->emailEdit->setReadOnly(readOnly);
    ui_->webPageEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void CounterpartyContactInformationDetailDialog::populateContactTypeCombo() {
    BOOST_LOG_SEV(lg(), debug) << "Populating contact_type combo";
    populateDynamicCombo<refdata::domain::contact_type>(
        ui_->contactTypeCombo,
        this,
        clientManager_,
        &fetch_contact_types,
        "contactTypeWatcher",
        [](const auto& t) { return QString::fromStdString(t.code); },
        [](const auto& t) { return QString::fromStdString(t.description); },
        [](const auto& t) { return t.display_order; },
        [this]() { return QString::fromStdString(counterpartyContactInformation_.contact_type); },
        [this](const QString& error) {
            emit errorMessage(tr("Failed to load contact types: %1").arg(error));
        },
        []() {},
        QObject::tr("Loading…"),
        QObject::tr("Failed to load"),
        [](const auto& t) { return QString::fromStdString(t.code); });
}
void CounterpartyContactInformationDetailDialog::updateUiFromInformation() {
    ui_->idEdit->setText(
        QString::fromStdString(boost::uuids::to_string(counterpartyContactInformation_.id)));
    {
        const auto val = QString::fromStdString(counterpartyContactInformation_.contact_type);
        const int idx = ui_->contactTypeCombo->findData(val);
        if (idx >= 0)
            ui_->contactTypeCombo->setCurrentIndex(idx);
    }
    ui_->streetLine1Edit->setText(
        QString::fromStdString(counterpartyContactInformation_.street_line_1));
    ui_->streetLine2Edit->setText(
        QString::fromStdString(counterpartyContactInformation_.street_line_2));
    ui_->cityEdit->setText(QString::fromStdString(counterpartyContactInformation_.city));
    ui_->stateEdit->setText(QString::fromStdString(counterpartyContactInformation_.state));
    {
        const auto val = QString::fromStdString(counterpartyContactInformation_.country_code);
        const int idx = ui_->countryCodeCombo->findText(val);
        ui_->countryCodeCombo->setCurrentIndex(idx);
    }
    ui_->postalCodeEdit->setText(
        QString::fromStdString(counterpartyContactInformation_.postal_code));
    ui_->phoneEdit->setText(QString::fromStdString(counterpartyContactInformation_.phone));
    ui_->emailEdit->setText(QString::fromStdString(counterpartyContactInformation_.email));
    ui_->webPageEdit->setText(QString::fromStdString(counterpartyContactInformation_.web_page));

    populateProvenance(counterpartyContactInformation_.version,
                       counterpartyContactInformation_.modified_by,
                       counterpartyContactInformation_.performed_by,
                       counterpartyContactInformation_.recorded_at,
                       counterpartyContactInformation_.change_reason_code,
                       counterpartyContactInformation_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void CounterpartyContactInformationDetailDialog::updateInformationFromUi() {
    counterpartyContactInformation_.contact_type =
        ui_->contactTypeCombo->currentText().toStdString();
    counterpartyContactInformation_.street_line_1 =
        ui_->streetLine1Edit->text().trimmed().toStdString();
    counterpartyContactInformation_.street_line_2 =
        ui_->streetLine2Edit->text().trimmed().toStdString();
    counterpartyContactInformation_.city = ui_->cityEdit->text().trimmed().toStdString();
    counterpartyContactInformation_.state = ui_->stateEdit->text().trimmed().toStdString();
    counterpartyContactInformation_.country_code =
        ui_->countryCodeCombo->currentText().toStdString();
    counterpartyContactInformation_.postal_code =
        ui_->postalCodeEdit->text().trimmed().toStdString();
    counterpartyContactInformation_.phone = ui_->phoneEdit->text().trimmed().toStdString();
    counterpartyContactInformation_.email = ui_->emailEdit->text().trimmed().toStdString();
    counterpartyContactInformation_.web_page = ui_->webPageEdit->text().trimmed().toStdString();
    counterpartyContactInformation_.modified_by = username_;
}

void CounterpartyContactInformationDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CounterpartyContactInformationDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CounterpartyContactInformationDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool CounterpartyContactInformationDetailDialog::validateInput() {
    const QString id_val = ui_->idEdit->text().trimmed();
    const bool contact_type_selected = ui_->contactTypeCombo->currentIndex() >= 0;

    return true && !id_val.isEmpty() && contact_type_selected;
}

void CounterpartyContactInformationDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this,
            "Disconnected",
            "Cannot save counterparty contact information while disconnected from server.");
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
    counterpartyContactInformation_.change_reason_code = crSel->reason_code;
    counterpartyContactInformation_.change_commentary = crSel->commentary;

    updateInformationFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving counterparty contact information: "
                              << counterpartyContactInformation_.contact_type;

    QPointer<CounterpartyContactInformationDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self,
                 counterpartyContactInformation = counterpartyContactInformation_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_counterparty_contact_information_request request;
        request.data = counterpartyContactInformation;
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
            BOOST_LOG_SEV(lg(), info) << "Counterparty Contact Information saved successfully";
            QString code =
                QString::fromStdString(self->counterpartyContactInformation_.contact_type);
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->counterpartyContactInformationSaved(code);
            self->notifySaveSuccess(tr("Counterparty Contact Information '%1' saved").arg(code));
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

void CounterpartyContactInformationDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this,
            "Disconnected",
            "Cannot delete counterparty contact information while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(counterpartyContactInformation_.contact_type);
    auto reply = MessageBoxHelper::question(
        this,
        "Delete Counterparty Contact Information",
        QString("Are you sure you want to delete counterparty contact information '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel =
        promptChangeReason(ChangeReasonDialog::OperationType::Delete, false, "common");
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting counterparty contact information: "
                              << counterpartyContactInformation_.contact_type;

    QPointer<CounterpartyContactInformationDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task =
        [self,
         id_str = boost::uuids::to_string(counterpartyContactInformation_.id)]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_counterparty_contact_information_request request;
        request.ids = {id_str};
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
            BOOST_LOG_SEV(lg(), info) << "Counterparty Contact Information deleted successfully";
            emit self->statusMessage(
                QString("Counterparty Contact Information '%1' deleted").arg(code));
            emit self->counterpartyContactInformationDeleted(code);
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
