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
#include "ores.qt/ComponentDialog.hpp"
#include "ores.synthetic.api/messaging/gmm_component_protocol.hpp"
#include <QDialogButtonBox>
#include <QFormLayout>
#include <QFutureWatcher>
#include <QMessageBox>
#include <QPointer>
#include <QVBoxLayout>
#include <QtConcurrent>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <utility>

namespace ores::qt {

using namespace ores::logging;

ComponentDialog::ComponentDialog(ClientManager* cm,
                                 const QString& username,
                                 const boost::uuids::uuid& parentFxId,
                                 int nextIndex,
                                 QWidget* parent)
    : QDialog(parent)
    , clientManager_(cm)
    , username_(username)
    , isNew_(true) {

    component_.id = boost::uuids::random_generator()();
    component_.fx_spot_config_id = parentFxId;
    component_.party_id = clientManager_->currentPartyId();
    component_.component_index = nextIndex;
    component_.mean = 0.0;
    component_.stdev = 0.001;
    component_.weight = 0.0;

    BOOST_LOG_SEV(lg(), info) << "Opening component dialog for a new component "
                              << boost::uuids::to_string(component_.id) << " under fx pair "
                              << boost::uuids::to_string(parentFxId) << ".";
    buildUi();
}

ComponentDialog::ComponentDialog(ClientManager* cm,
                                 const QString& username,
                                 const synthetic::domain::gmm_component& existing,
                                 QWidget* parent)
    : QDialog(parent)
    , clientManager_(cm)
    , username_(username)
    , isNew_(false)
    , component_(existing) {

    BOOST_LOG_SEV(lg(), info) << "Opening component dialog editing component "
                              << boost::uuids::to_string(component_.id) << ".";
    buildUi();
}

void ComponentDialog::buildUi() {
    setWindowTitle(isNew_ ? tr("New Component") : tr("Edit Component"));
    setModal(true);

    auto* layout = new QVBoxLayout(this);
    auto* form = new QFormLayout();

    indexSpin_ = new QSpinBox(this);
    indexSpin_->setRange(0, 100000);
    indexSpin_->setValue(component_.component_index);

    meanSpin_ = new QDoubleSpinBox(this);
    meanSpin_->setRange(-1e9, 1e9);
    meanSpin_->setDecimals(6);
    meanSpin_->setValue(component_.mean);

    stdevSpin_ = new QDoubleSpinBox(this);
    stdevSpin_->setRange(0.0000001, 1e9);
    stdevSpin_->setDecimals(7);
    stdevSpin_->setValue(component_.stdev > 0 ? component_.stdev : 0.001);

    weightSpin_ = new QDoubleSpinBox(this);
    weightSpin_->setRange(0.0, 1e9);
    weightSpin_->setDecimals(6);
    weightSpin_->setValue(component_.weight);

    form->addRow(tr("Component index"), indexSpin_);
    form->addRow(tr("Mean"), meanSpin_);
    form->addRow(tr("Stdev"), stdevSpin_);
    form->addRow(tr("Weight"), weightSpin_);
    layout->addLayout(form);

    auto* buttons = new QDialogButtonBox(QDialogButtonBox::Save | QDialogButtonBox::Cancel, this);
    layout->addWidget(buttons);
    connect(buttons, &QDialogButtonBox::accepted, this, &ComponentDialog::onSave);
    connect(buttons, &QDialogButtonBox::rejected, this, &QDialog::reject);
}

void ComponentDialog::onSave() {
    auto c = component_;
    c.component_index = indexSpin_->value();
    c.mean = meanSpin_->value();
    c.stdev = stdevSpin_->value();
    c.weight = weightSpin_->value();
    c.modified_by = username_.toStdString();
    c.change_reason_code = isNew_ ? "system.new_record" : "common.non_material_update";
    c.change_commentary = "Authored via Market Simulator";
    c.version = 0;

    const std::string id = boost::uuids::to_string(c.id);
    BOOST_LOG_SEV(lg(), info) << "Saving component " << id << " (new=" << isNew_ << ").";

    QPointer<ComponentDialog> self = this;
    auto* cm = clientManager_;

    auto task = [cm, c]() -> std::pair<bool, QString> {
        auto resp = cm->process_authenticated_request(
            synthetic::messaging::save_gmm_component_request::from(c));
        if (!resp)
            return {false, QString::fromStdString(resp.error())};
        if (!resp->success)
            return {false, QString::fromStdString(resp->message)};
        return {true, {}};
    };

    auto* watcher = new QFutureWatcher<std::pair<bool, QString>>(self);
    connect(watcher, &QFutureWatcher<std::pair<bool, QString>>::finished, self,
            [self, watcher, id]() {
                auto [ok, err] = watcher->result();
                watcher->deleteLater();
                if (!self)
                    return;
                if (!ok) {
                    BOOST_LOG_SEV(lg(), error)
                        << "Save failed for component " << id << ": " << err.toStdString();
                    QMessageBox::critical(self, self->tr("Save failed"), err);
                    return;
                }
                BOOST_LOG_SEV(lg(), info) << "Saved component " << id << ".";
                self->accept();
            });
    watcher->setFuture(QtConcurrent::run(task));
}

}
